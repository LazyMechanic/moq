#![allow(dead_code)]

use quote::format_ident;
use replace_with::replace_with_or_abort;
use syn::{
    parse_quote, punctuated::Punctuated, visit_mut, visit_mut::VisitMut, Attribute, BoundLifetimes,
    FnArg, GenericParam, Generics, Ident, ImplItem, ItemTrait, PathArguments, PathSegment, QSelf,
    ReturnType, Token, TraitItem, TraitItemFn, Type, TypeParamBound, TypePath, WhereClause,
    WherePredicate,
};

use crate::{
    attribute::{AttributePresent, MoqAttribute, OutputAttributeValue},
    context::Context,
    symbols,
};

pub fn format_mock_ident(trait_def: &ItemTrait) -> Result<Ident, syn::Error> {
    let mut attr_present = AttributePresent::default();
    let mut ident = None;
    for attr in trait_def.attrs.moqified_iter() {
        let nested_list =
            attr.parse_args_with(Punctuated::<MoqAttribute, Token![,]>::parse_terminated)?;
        for nested in nested_list {
            match nested {
                // #[moq(rename = "MockIdent")]
                MoqAttribute::Rename(attr) => {
                    attr_present.check_and_hit(&attr)?;
                    ident = Some(attr.into_value());
                }
                other => return Err(other.unsupported_error()),
            }
        }
    }
    Ok(ident.unwrap_or_else(|| format_ident!("Mock{}", trait_def.ident)))
}

pub fn format_action_collection_ident(mock_ident: &Ident) -> Ident {
    format_ident!("__{}_ActionCollection", mock_ident)
}

pub fn format_action_ident(mock_ident: &Ident, func_ident: &Ident) -> Ident {
    format_ident!("__{}_{}_Action", mock_ident, func_ident)
}

/// Generate `Func/AsyncFunc + Send + 'static` trait bound
pub fn make_exp_func_trait_bound(
    cx: &Context,
    trait_func: &TraitItemFn,
) -> Result<Punctuated<TypeParamBound, Token![+]>, syn::Error> {
    let is_async = trait_func.sig.asyncness.is_some();

    // Filter lifetimes from generics
    let lts = trait_func
        .sig
        .generics
        .lifetimes()
        .map(|lt| lt.lifetime.clone())
        .collect::<Vec<_>>();

    // Filters function arguments, except self arg
    let args_ty_iter = trait_func
        .sig
        .inputs
        .iter()
        .filter_map(|inp| match inp {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pt) => Some(&*pt.ty),
        })
        .cloned()
        .map(|ty| ty.deselfified(cx));

    let ret_ty = make_action_call_func_ret(cx, trait_func)?.unwrap_or(parse_quote! { () });
    let lts_bounds: Option<BoundLifetimes> = if lts.is_empty() {
        None
    } else {
        Some(parse_quote! { for<#(#lts),*> })
    };

    let func_trait: Type = if is_async {
        parse_quote! { ::moq::AsyncFunc<(#(#args_ty_iter,)*), #ret_ty> }
    } else {
        parse_quote! { ::moq::Func<(#(#args_ty_iter,)*), #ret_ty> }
    };

    let res = parse_quote! {
        #lts_bounds #func_trait
            + ::std::marker::Send
            + ::std::marker::Sync
            + 'static
    };

    Ok(res)
}

pub fn make_action_call_func_ret(
    cx: &Context,
    trait_func: &TraitItemFn,
) -> Result<Option<Type>, syn::Error> {
    match &trait_func.sig.output {
        ReturnType::Default => Ok(None),
        ReturnType::Type(_, ty) => match &**ty {
            Type::ImplTrait(ty) => {
                let moq_attrs_iter = trait_func.attrs.moqified_iter();
                let bounds = &ty.bounds;

                let default_ty: Type = parse_quote!( ::std::boxed::Box<dyn #bounds> );
                let mut attr_present = AttributePresent::default();
                let mut ret_ty = None;
                for attr in moq_attrs_iter {
                    let nested_list = attr
                        .parse_args_with(Punctuated::<MoqAttribute, Token![,]>::parse_terminated)?;
                    for nested in nested_list {
                        match nested {
                            MoqAttribute::Output(attr) => {
                                attr_present.check_and_hit(&attr)?;

                                match attr.value() {
                                    OutputAttributeValue::FullPath(path) => {
                                        ret_ty = Some(parse_quote! { #path });
                                    }
                                    OutputAttributeValue::InferPath(path) => {
                                        ret_ty = Some(parse_quote! { #path<#default_ty> });
                                    }
                                }
                            }
                            other => return Err(other.unsupported_error()),
                        }
                    }
                }
                let ret_ty = ret_ty
                    .unwrap_or_else(|| {
                        if is_pin_boxed(bounds) {
                            parse_quote! { ::std::pin::Pin<#default_ty> }
                        } else {
                            default_ty
                        }
                    })
                    .deselfified(cx);
                Ok(Some(ret_ty))
            }
            other => {
                let ret_ty = other.clone().deselfified(cx);
                Ok(Some(ret_ty))
            }
        },
    }
}

fn is_pin_boxed(bounds: &Punctuated<TypeParamBound, Token![+]>) -> bool {
    fn is_pin_boxed_type(ident: &Ident) -> bool {
        ident == "Future" || ident == "Stream"
    }

    let Some(first) = bounds.first() else {
        return false;
    };
    let TypeParamBound::Trait(trai) = first else {
        return false;
    };
    trai.path
        .segments
        .last()
        .map_or(false, |ts| is_pin_boxed_type(&ts.ident))
}

// ======================= EXT TRAITS ======================= //

pub trait Demoqifing {
    type IterItem;
    fn demoqified_iter(&self) -> impl Iterator<Item = &Self::IterItem> {
        std::iter::empty()
    }
    fn demoqified(mut self) -> Self
    where
        Self: Sized,
    {
        self.demoqify();
        self
    }
    fn demoqify(&mut self);
}

pub trait Moqifing {
    type IterItem;
    fn moqified_iter(&self) -> impl Iterator<Item = &Self::IterItem> {
        std::iter::empty()
    }
    fn moqified(mut self) -> Self
    where
        Self: Sized,
    {
        self.moqify();
        self
    }
    fn moqify(&mut self);
}

pub trait Merging {
    fn merged(mut self, other: Self) -> Self
    where
        Self: Sized,
    {
        self.merge(other);
        self
    }
    fn merge(&mut self, other: Self);
}

pub trait Lifetimifing {
    fn lifetimified(mut self) -> Self
    where
        Self: Sized,
    {
        self.lifetimify();
        self
    }
    fn lifetimify(&mut self);
}

pub trait Delifetimifing {
    fn delifetimified(mut self) -> Self
    where
        Self: Sized,
    {
        self.delifetimify();
        self
    }
    fn delifetimify(&mut self);
}

pub trait Staticizing {
    fn staticized(mut self) -> Self
    where
        Self: Sized,
    {
        self.staticize();
        self
    }
    fn staticize(&mut self);
}

pub trait Deselfifing {
    fn deselfified(mut self, cx: &Context) -> Self
    where
        Self: Sized,
    {
        self.deselfify(cx);
        self
    }
    fn deselfify(&mut self, cx: &Context);
}

// ======================= TRAIT ITEM ======================= //

impl Demoqifing for TraitItem {
    type IterItem = ();

    fn demoqify(&mut self) {
        let mut vis = DemoqifyVisitor;
        vis.visit_trait_item_mut(self);
    }
}

// ======================= ITEM TRAIT ======================= //

impl Demoqifing for ItemTrait {
    type IterItem = ();

    fn demoqify(&mut self) {
        let mut vis = DemoqifyVisitor;
        vis.visit_item_trait_mut(self);
    }
}

// ======================= IMPL ITEM ======================= //

impl Deselfifing for ImplItem {
    fn deselfify(&mut self, cx: &Context) {
        let mut vis = DeselfifyVisitor { cx };
        vis.visit_impl_item_mut(self);
    }
}

// ======================= ATTRIBUTES ======================= //

impl Moqifing for Vec<Attribute> {
    type IterItem = Attribute;

    fn moqified_iter(&self) -> impl Iterator<Item = &Self::IterItem> {
        self.iter().filter(|attr| attr.path() == symbols::MOQ)
    }

    fn moqify(&mut self) {
        let mut vis = MoqifyVisitor;
        vis.visit_attributes_mut(self);
    }
}

impl Demoqifing for Vec<Attribute> {
    type IterItem = Attribute;

    fn demoqified_iter(&self) -> impl Iterator<Item = &Self::IterItem> {
        self.iter().filter(|attr| attr.path() != symbols::MOQ)
    }

    fn demoqify(&mut self) {
        let mut vis = DemoqifyVisitor;
        vis.visit_attributes_mut(self);
    }
}

// ======================= GENERICS ======================= //

impl Merging for Generics {
    fn merge(&mut self, other: Self) {
        merge_generics(self, other);
    }
}

impl Lifetimifing for Generics {
    fn lifetimify(&mut self) {
        let mut vis = LifetimifyVisitor;
        vis.visit_generics_mut(self);
    }
}

impl Delifetimifing for Generics {
    fn delifetimify(&mut self) {
        let mut vis = DelifetimifyVisitor;
        vis.visit_generics_mut(self);
    }
}

impl Staticizing for Generics {
    fn staticize(&mut self) {
        let mut vis = StaticizeVisitor;
        vis.visit_generics_mut(self);
    }
}

/// Merge two generics inplace into one
fn merge_generics(dst: &mut Generics, other: Generics) {
    dst.params.extend(other.params);
    match (&mut dst.where_clause, other.where_clause) {
        (Some(dst_where_clause), Some(other_where_clause)) => {
            dst_where_clause
                .predicates
                .extend(other_where_clause.predicates);
        }
        (Some(_), None) => { /* do nothing */ }
        (None, Some(other_where_clause)) => dst.where_clause = Some(other_where_clause),
        (None, None) => { /* do nothing */ }
    }

    dst.lt_token = dst.lt_token.or(other.lt_token);
    dst.gt_token = dst.gt_token.or(other.gt_token);
}

// ======================= TYPE ======================= //

impl Deselfifing for Type {
    fn deselfify(&mut self, cx: &Context) {
        let mut vis = DeselfifyVisitor { cx };
        vis.visit_type_mut(self);
    }
}

// ======================= VISITORS ======================= //

struct LifetimifyVisitor;

impl VisitMut for LifetimifyVisitor {
    fn visit_generics_mut(&mut self, i: &mut Generics) {
        replace_with_or_abort(i, |mut gen| {
            gen.params = gen
                .params
                .into_iter()
                .filter(|p| matches!(p, GenericParam::Lifetime(_)))
                .collect();

            gen.where_clause = gen.where_clause.map(|mut wc| {
                wc.predicates = wc
                    .predicates
                    .into_iter()
                    .filter_map(|pred| match pred {
                        x @ WherePredicate::Lifetime(_) => Some(x),
                        _ => None,
                    })
                    .collect();
                wc
            });

            gen
        });
    }
}

struct DelifetimifyVisitor;

impl VisitMut for DelifetimifyVisitor {
    fn visit_generics_mut(&mut self, i: &mut Generics) {
        replace_with_or_abort(i, |mut gen| {
            gen.params = gen
                .params
                .into_iter()
                .filter(|p| !matches!(p, GenericParam::Lifetime(_)))
                .collect();

            gen.where_clause = gen.where_clause.map(|mut wc| {
                wc.predicates = wc
                    .predicates
                    .into_iter()
                    .filter_map(|pred| match pred {
                        WherePredicate::Type(mut ty) => {
                            ty.bounds = ty
                                .bounds
                                .into_iter()
                                .filter(|bound| !matches!(bound, TypeParamBound::Lifetime(_)))
                                .collect();
                            Some(WherePredicate::Type(ty))
                        }
                        WherePredicate::Lifetime(_) => None,
                        _ => None,
                    })
                    .collect();
                wc
            });

            gen
        });
    }
}

struct StaticizeVisitor;

impl VisitMut for StaticizeVisitor {
    fn visit_generics_mut(&mut self, i: &mut Generics) {
        let ident_iter = i.params.iter().filter_map(|p| match p {
            GenericParam::Type(ty) => Some(&ty.ident),
            _ => None,
        });

        let where_clause = i.where_clause.get_or_insert(WhereClause {
            where_token: <Token![where]>::default(),
            predicates: Punctuated::default(),
        });
        for ident in ident_iter {
            let p = parse_quote! { #ident: 'static };
            where_clause.predicates.push(p);
        }
    }
}

struct DemoqifyVisitor;

impl VisitMut for DemoqifyVisitor {
    fn visit_attributes_mut(&mut self, i: &mut Vec<Attribute>) {
        replace_with_or_abort(i, |i| {
            i.into_iter()
                .filter(|attr| attr.path() != symbols::MOQ)
                .collect()
        });
    }
}

struct MoqifyVisitor;

impl VisitMut for MoqifyVisitor {
    fn visit_attributes_mut(&mut self, i: &mut Vec<Attribute>) {
        replace_with_or_abort(i, |i| {
            i.into_iter()
                .filter(|attr| attr.path() == symbols::MOQ)
                .collect()
        });
    }
}

/// Turns `Self::AssocItem` -> `<MockTrait as Trait>::AssocItem`
struct DeselfifyVisitor<'a> {
    cx: &'a Context,
}

impl VisitMut for DeselfifyVisitor<'_> {
    fn visit_fn_arg_mut(&mut self, i: &mut FnArg) {
        match i {
            FnArg::Receiver(_) => { /* do nothing */ }
            FnArg::Typed(pat) => self.visit_pat_type_mut(pat),
        }
    }

    fn visit_type_path_mut(&mut self, i: &mut TypePath) {
        for seg in &mut i.path.segments {
            // If the segment is `Self` (first segment) then init qself with mock struct
            // and set the first segment is trait.
            // The result will be looks like `Self::AssocItem` -> `<MockTrait<T> as Trait<T>>::AssocItem`
            //
            // If qself is already exists then nothing will happen because `Self` can't be in segments
            if seg.ident == "Self" {
                let mock_ident = &self.cx.mock_ident;
                let (_mock_impl_generics, mock_ty_generics, _mock_where_clause) =
                    self.cx.mock_generics.split_for_impl();
                i.qself = Some(QSelf {
                    lt_token: <Token![<]>::default(),
                    ty: Box::new(parse_quote! { #mock_ident #mock_ty_generics }),
                    position: 1,
                    as_token: Some(<Token![as]>::default()),
                    gt_token: <Token![>]>::default(),
                });

                let trait_segment = if self.cx.trait_def.generics.params.is_empty() {
                    PathSegment {
                        ident: self.cx.trait_def.ident.clone(),
                        arguments: PathArguments::None,
                    }
                } else {
                    let (_trait_impl_generics, trait_ty_generics, _trait_where_clause) =
                        self.cx.trait_def.generics.split_for_impl();
                    PathSegment {
                        ident: self.cx.trait_def.ident.clone(),
                        arguments: PathArguments::AngleBracketed(
                            parse_quote! { #trait_ty_generics },
                        ),
                    }
                };

                // Replace `Self` segment to the new one
                *seg = trait_segment;
            }
        }

        // Delegate to the default impl to visit nested expressions.
        visit_mut::visit_type_path_mut(self, i);
    }
}
