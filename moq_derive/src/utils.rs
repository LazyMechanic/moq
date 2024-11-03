use crate::attribute::{AttributePresent, MoqAttribute, OutputAttribute};
use crate::context::Context;
use crate::symbols;
use quote::format_ident;
use replace_with::replace_with_or_abort;
use syn::punctuated::Punctuated;
use syn::visit_mut::VisitMut;
use syn::{
    parse_quote, Attribute, BoundLifetimes, FnArg, GenericParam, Generics, Ident, ImplItem,
    ItemTrait, PathArguments, PathSegment, QSelf, ReturnType, Token, TraitItem, TraitItemFn, Type,
    TypeParamBound, TypePath, WhereClause, WherePredicate,
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
                    ident = Some(attr.ident);
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

/// Generate `Box<dyn Any>` type
pub fn make_boxed_any() -> Type {
    parse_quote! {
        ::std::boxed::Box<dyn ::std::any::Any
                            + ::std::marker::Send
                            + ::std::marker::Sync
                            + 'static>
    }
}

/// Generate `Box<dyn Func/AsyncFunc>` type
pub fn make_boxed_exp_func(cx: &Context, trait_func: &TraitItemFn) -> Result<Type, syn::Error> {
    let bound = make_exp_func_trait_bound(cx, trait_func)?;
    let ty = parse_quote! { ::std::boxed::Box<dyn #bound> };
    Ok(ty)
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

                                match attr {
                                    OutputAttribute::FullPath(path) => {
                                        ret_ty = Some(parse_quote! { #path });
                                    }
                                    OutputAttribute::InferPath(path) => {
                                        ret_ty = Some(parse_quote! { #path<#default_ty> });
                                    }
                                }
                            }
                            other => return Err(other.unsupported_error()),
                        }
                    }
                }
                let ret_ty = ret_ty.unwrap_or(default_ty).deselfified(cx);
                Ok(Some(ret_ty))
            }
            other => {
                let ret_ty = other.clone().deselfified(cx);
                Ok(Some(ret_ty))
            }
        },
    }
}

// ======================= TRAIT ITEM ======================= //

pub trait TraitItemExt {
    fn demoqified(self) -> Self;
    fn demoqify(&mut self);
}

impl TraitItemExt for TraitItem {
    fn demoqified(mut self) -> Self {
        self.demoqify();
        self
    }

    fn demoqify(&mut self) {
        replace_with_or_abort(self, |this| match this {
            TraitItem::Const(mut item) => {
                item.attrs.demoqify();
                TraitItem::Const(item)
            }
            TraitItem::Fn(mut item) => {
                item.attrs.demoqify();
                TraitItem::Fn(item)
            }
            TraitItem::Type(mut item) => {
                item.attrs.demoqify();
                TraitItem::Type(item)
            }
            TraitItem::Macro(mut item) => {
                item.attrs.demoqify();
                TraitItem::Macro(item)
            }
            item => item,
        });
    }
}

// ======================= ITEM TRAIT ======================= //

pub trait ItemTraitExt {
    fn demoqified(self) -> Self;
    fn demoqify(&mut self);
}

impl ItemTraitExt for ItemTrait {
    fn demoqified(mut self) -> Self {
        self.demoqify();
        self
    }

    fn demoqify(&mut self) {
        self.items.iter_mut().for_each(|item| item.demoqify());
    }
}

// ======================= IMPL ITEM ======================= //

pub trait ImplItemExt {
    fn deselfified(self, cx: &Context) -> Self;
    fn deselfify(&mut self, cx: &Context);
}

impl ImplItemExt for ImplItem {
    fn deselfified(mut self, cx: &Context) -> Self {
        self.deselfify(cx);
        self
    }

    fn deselfify(&mut self, cx: &Context) {
        let mut vis = DeselfifyVisitor { cx };
        vis.visit_impl_item_mut(self);
    }
}

// ======================= ATTRIBUTES ======================= //

pub trait AttributesExt {
    fn moqified_iter(&self) -> impl Iterator<Item = &Attribute>;
    fn moqified(self) -> Self;
    fn moqify(&mut self);
    fn demoqified_iter(&self) -> impl Iterator<Item = &Attribute>;
    fn demoqified(self) -> Self;
    fn demoqify(&mut self);
}

impl AttributesExt for Vec<Attribute> {
    fn moqified_iter(&self) -> impl Iterator<Item = &Attribute> {
        self.iter().filter(|attr| attr.path() == symbols::MOQ)
    }

    fn moqified(mut self) -> Self {
        self.moqify();
        self
    }

    fn moqify(&mut self) {
        replace_with_or_abort(self, |this| {
            this.into_iter()
                .filter(|attr| attr.path() == symbols::MOQ)
                .collect()
        });
    }

    fn demoqified_iter(&self) -> impl Iterator<Item = &Attribute> {
        self.iter().filter(|attr| attr.path() != symbols::MOQ)
    }

    fn demoqified(mut self) -> Self {
        self.moqify();
        self
    }

    fn demoqify(&mut self) {
        replace_with_or_abort(self, |this| {
            this.into_iter()
                .filter(|attr| attr.path() != symbols::MOQ)
                .collect()
        });
    }
}

// ======================= GENERICS ======================= //

pub trait GenericsExt {
    /// Merge two generics into one
    fn merged(self, other: Generics) -> Generics;
    /// Merge two generics into one
    fn merge(&mut self, other: Generics);
    /// Filters generics for lifetimes and returns new generics with only lifetimes
    fn lifetimified(self) -> Generics;
    /// Filters generics for lifetimes and leaves generics only with lifetimes
    fn lifetimify(&mut self);
    /// Removes lifetimes from generics
    fn delifetimified(self) -> Generics;
    /// Removes lifetimes from generics
    fn delifetimify(&mut self);
    /// Applies `T: 'static` bound on every generic parameter
    fn staticized(self) -> Generics;
    /// Applies `T: 'static` bound on every generic parameter
    fn staticize(&mut self);
}

impl GenericsExt for Generics {
    fn merged(mut self, other: Generics) -> Generics {
        self.merge(other);
        self
    }

    fn merge(&mut self, other: Generics) {
        merge_generics(self, other);
    }

    fn lifetimified(mut self) -> Generics {
        self.lifetimify();
        self
    }

    fn lifetimify(&mut self) {
        lifetimify_generics(self);
    }

    fn delifetimified(mut self) -> Generics {
        self.delifetimify();
        self
    }

    fn delifetimify(&mut self) {
        delifetimify_generics(self);
    }

    fn staticized(mut self) -> Generics {
        self.staticize();
        self
    }

    fn staticize(&mut self) {
        staticize_generics(self);
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

fn lifetimify_generics(gen: &mut Generics) {
    gen.params = std::mem::take(&mut gen.params)
        .into_iter()
        .filter(|p| matches!(p, GenericParam::Lifetime(_)))
        .collect();

    gen.where_clause = std::mem::take(&mut gen.where_clause).map(|mut wc| {
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
}

/// Removes lifetimes from generics
fn delifetimify_generics(gen: &mut Generics) {
    gen.params = std::mem::take(&mut gen.params)
        .into_iter()
        .filter(|p| !matches!(p, GenericParam::Lifetime(_)))
        .collect();

    gen.where_clause = std::mem::take(&mut gen.where_clause).map(|mut wc| {
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
}

fn staticize_generics(gen: &mut Generics) {
    let ident_iter = gen.params.iter().filter_map(|p| match p {
        GenericParam::Type(ty) => Some(&ty.ident),
        _ => None,
    });

    let where_clause = gen.where_clause.get_or_insert(WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::default(),
    });
    for ident in ident_iter {
        let p = parse_quote! { #ident: 'static };
        where_clause.predicates.push(p);
    }
}

// ======================= TYPE ======================= //

pub trait TypeExt {
    fn deselfified(self, cx: &Context) -> Self;
    fn deselfify(&mut self, cx: &Context);
}

impl TypeExt for Type {
    fn deselfified(mut self, cx: &Context) -> Self {
        self.deselfify(cx);
        self
    }

    fn deselfify(&mut self, cx: &Context) {
        let mut vis = DeselfifyVisitor { cx };
        vis.visit_type_mut(self);
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
        if let Some(qself) = &mut i.qself {
            self.visit_qself_mut(qself);
        }

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
    }
}
