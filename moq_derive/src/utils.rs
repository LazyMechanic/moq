use crate::context::Context;
use crate::symbols;
use if_chain::if_chain;
use itertools::Itertools;
use quote::{format_ident, quote};
use replace_with::replace_with_or_abort;
use std::borrow::Borrow;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, BoundLifetimes, Expr, ExprLit, FnArg, GenericArgument, GenericParam,
    Generics, Ident, ImplItem, ItemTrait, Lit, MetaNameValue, Path, PathArguments, PathSegment,
    QSelf, ReturnType, Token, TraitItem, TraitItemFn, Type, TypeParamBound, WhereClause,
    WherePredicate,
};

pub fn format_mock_ident(trait_ident: &Ident) -> Ident {
    format_ident!("{}Mock", trait_ident)
}

pub fn format_action_collection_ident(trait_ident: &Ident) -> Ident {
    let mock_ident = format_mock_ident(trait_ident);
    format_ident!("__{}_ActionCollection", mock_ident)
}

pub fn format_action_ident(trait_ident: &Ident, func_ident: &Ident) -> Ident {
    let mock_ident = format_mock_ident(trait_ident);
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
    let args_ty = trait_func
        .sig
        .inputs
        .iter()
        .filter_map(|inp| match inp {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pt) => Some(&*pt.ty),
        })
        .cloned()
        .map(|mut ty| ty.deselfified(cx))
        .collect::<Result<Vec<_>, _>>()?;

    let ret_ty = make_action_call_func_ret(cx, trait_func)?.unwrap_or(parse_quote! { () });
    let lts_bounds: Option<BoundLifetimes> = if lts.is_empty() {
        None
    } else {
        Some(parse_quote! { for<#(#lts),*> })
    };

    let func_trait: Type = if is_async {
        parse_quote! { ::moq::AsyncFunc<(#(#args_ty,)*), #ret_ty> }
    } else {
        parse_quote! { ::moq::Func<(#(#args_ty,)*), #ret_ty> }
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
                // TODO: check count of identical attributes
                let mut ret_ty: Type = parse_quote!( ::std::boxed::Box<dyn #bounds> );
                for attr in moq_attrs_iter {
                    let nested_list = attr.parse_args_with(
                        Punctuated::<MetaNameValue, Token![,]>::parse_terminated,
                    )?;
                    for nested in nested_list {
                        if nested.path == symbols::OUTPUT {
                            // #[moq(return = "::path::to::Type")]
                            // #[moq(return = "::path::to::Type<_>")]
                            match nested.value {
                                Expr::Lit(ExprLit {
                                    lit: Lit::Str(lit), ..
                                }) => {
                                    let path: Path = lit.parse()?;
                                    let last_segment = path.segments.last().ok_or_else(|| {
                                        syn::Error::new_spanned(
                                            &path.segments,
                                            "last segment not found",
                                        )
                                    })?;

                                    // #[moq(return = "::path::to::Type<_>")]
                                    if_chain! {
                                        if let PathArguments::AngleBracketed(args) = &last_segment.arguments;
                                        if args.args.len() == 1;
                                        if matches!(
                                            args.args.first(),
                                            Some(GenericArgument::Type(Type::Infer(_)))
                                        );
                                        then {
                                            // ::path::to::Type<Box<dyn ...>>
                                            let wrapper = {
                                                let mut p = path.clone();
                                                let last_segment =
                                                    p.segments.last_mut().expect("already checked");
                                                last_segment.arguments = PathArguments::None;
                                                quote!{ #p }
                                            };
                                            ret_ty = parse_quote! { #wrapper<#ret_ty> };
                                        } else {
                                            ret_ty = parse_quote! { #path };
                                        }
                                    }
                                }
                                other => {
                                    return Err(syn::Error::new_spanned(
                                        other,
                                        "unsupported attribute value format",
                                    ))
                                }
                            }
                        } else {
                            return Err(syn::Error::new_spanned(attr, "unsupported attribute"));
                        }
                    }
                }
                ret_ty.deselfify(cx)?;
                Ok(Some(ret_ty))
            }
            other => {
                let ret_ty = other.clone().deselfified(cx)?;
                Ok(Some(ret_ty))
            }
        },
    }
}

/// Removes `#[moq(..)]` attributes
pub fn demoqify_trait(mut trait_def: ItemTrait) -> ItemTrait {
    trait_def.items = trait_def
        .items
        .into_iter()
        .map(|item| match item {
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
        })
        .collect();

    trait_def
}

pub fn deselfify_impl_item(cx: &Context, item: ImplItem) -> Result<ImplItem, syn::Error> {
    match item {
        ImplItem::Const(cst) => {
            // TODO
            Ok(ImplItem::Const(cst))
        }
        ImplItem::Fn(mut f) => {
            for inp in &mut f.sig.inputs {
                match inp {
                    FnArg::Receiver(_) => {}
                    FnArg::Typed(arg) => {
                        arg.ty.deselfify(cx)?;
                    }
                }
            }

            if let ReturnType::Type(_, ty) = &mut f.sig.output {
                ty.deselfify(cx)?;
            }

            Ok(ImplItem::Fn(f))
        }
        ImplItem::Type(ty) => {
            // TODO
            Ok(ImplItem::Type(ty))
        }
        other => Ok(other),
    }
}

// ======================= ATTRIBUTES ======================= //

pub trait AttributesExt {
    fn moqified_iter(&self) -> impl Iterator<Item = &Attribute>;
    fn moqified(self) -> Self;
    fn moqify(&mut self) -> &mut Self;
    fn demoqified_iter(&self) -> impl Iterator<Item = &Attribute>;
    fn demoqified(self) -> Self;
    fn demoqify(&mut self) -> &mut Self;
}

impl AttributesExt for Vec<Attribute> {
    fn moqified_iter(&self) -> impl Iterator<Item = &Attribute> {
        self.iter().filter(|attr| attr.path() == symbols::MOQ)
    }

    fn moqified(mut self) -> Self {
        self.moqify();
        self
    }

    fn moqify(&mut self) -> &mut Self {
        replace_with_or_abort(self, |this| {
            this.into_iter()
                .filter(|attr| attr.path() == symbols::MOQ)
                .collect()
        });
        self
    }

    fn demoqified_iter(&self) -> impl Iterator<Item = &Attribute> {
        self.iter().filter(|attr| attr.path() != symbols::MOQ)
    }

    fn demoqified(mut self) -> Self {
        self.moqify();
        self
    }

    fn demoqify(&mut self) -> &mut Self {
        replace_with_or_abort(self, |this| {
            this.into_iter()
                .filter(|attr| attr.path() != symbols::MOQ)
                .collect()
        });
        self
    }
}

// ======================= GENERICS ======================= //

pub trait GenericsExt {
    /// Merge two generics into one
    fn merged(self, other: Generics) -> Generics;
    /// Merge two generics into one
    fn merge(&mut self, other: Generics) -> &mut Self;
    /// Filters generics for lifetimes and returns new generics with only lifetimes
    fn lifetimified(self) -> Generics;
    /// Filters generics for lifetimes and leaves generics only with lifetimes
    fn lifetimify(&mut self) -> &mut Self;
    /// Removes lifetimes from generics
    fn delifetimified(self) -> Generics;
    /// Removes lifetimes from generics
    fn delifetimify(&mut self) -> &mut Self;
    /// Applies `T: 'static` bound on every generic parameter
    fn staticized(self) -> Generics;
    /// Applies `T: 'static` bound on every generic parameter
    fn staticize(&mut self) -> &mut Self;
}

impl GenericsExt for Generics {
    fn merged(mut self, other: Generics) -> Generics {
        self.merge(other);
        self
    }

    fn merge(&mut self, other: Generics) -> &mut Self {
        merge_generics(self, other);
        self
    }

    fn lifetimified(mut self) -> Generics {
        self.lifetimify();
        self
    }

    fn lifetimify(&mut self) -> &mut Self {
        lifetimify_generics(self);
        self
    }

    fn delifetimified(mut self) -> Generics {
        self.delifetimify();
        self
    }

    fn delifetimify(&mut self) -> &mut Self {
        delifetimify_generics(self);
        self
    }

    fn staticized(mut self) -> Generics {
        self.staticize();
        self
    }

    fn staticize(&mut self) -> &mut Self {
        staticize_generics(self);
        self
    }
}

/// Merge two generics inplace into one
fn merge_generics(dst: &mut Generics, other: Generics) {
    dst.params.extend(other.params.into_iter());
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

// ======================= GENERICS ======================= //

pub trait TypeExt {
    fn deselfified(self, cx: &Context) -> Result<Self, syn::Error>
    where
        Self: Sized;
    fn deselfify(&mut self, cx: &Context) -> Result<&mut Self, syn::Error>;
}

impl TypeExt for Type {
    fn deselfified(mut self, cx: &Context) -> Result<Self, syn::Error>
    where
        Self: Sized,
    {
        self.deselfify(cx)?;
        Ok(self)
    }

    fn deselfify(&mut self, cx: &Context) -> Result<&mut Self, syn::Error> {
        deselfify_type(self, cx)?;
        Ok(self)
    }
}

/// Turns `Self::AssocItem` -> `<TraitMock as Trait>::AssocItem`
fn deselfify_type(ty: &mut Type, cx: &Context) -> Result<(), syn::Error> {
    match ty {
        Type::Array(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::BareFn(ty) => {
            if let ReturnType::Type(_, ty) = &mut ty.output {
                deselfify_type(&mut **ty, cx)?;
            }
            for inp_arg in &mut ty.inputs {
                deselfify_type(&mut inp_arg.ty, cx)?;
            }
        }
        Type::Group(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::Paren(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::Path(ty) => {
            if let Some(qself) = &mut ty.qself {
                deselfify_type(&mut *qself.ty, cx)?
            }

            for seg in &mut ty.path.segments {
                // If the segment is `Self` (first segment) then init qself with mock struct
                // and set the first segment is trait.
                // The result will be looks like `Self::AssocItem` -> `<TraitMock<T> as Trait<T>>::AssocItem`
                //
                // If qself is already exists then nothing will happen because `Self` can't  be in segments
                if seg.ident == "Self" {
                    let mock_ident = &cx.mock_ident;
                    let (_mock_impl_generics, mock_ty_generics, _mock_where_clause) =
                        cx.mock_generics.split_for_impl();
                    ty.qself = Some(QSelf {
                        lt_token: <Token![<]>::default(),
                        ty: Box::new(parse_quote! { #mock_ident #mock_ty_generics }),
                        position: 1,
                        as_token: Some(<Token![as]>::default()),
                        gt_token: <Token![>]>::default(),
                    });

                    let trait_segment = if cx.trait_generics.params.is_empty() {
                        PathSegment {
                            ident: cx.trait_ident.clone(),
                            arguments: PathArguments::None,
                        }
                    } else {
                        let (_trait_impl_generics, trait_ty_generics, _trait_where_clause) =
                            cx.trait_generics.split_for_impl();
                        PathSegment {
                            ident: cx.trait_ident.clone(),
                            arguments: PathArguments::AngleBracketed(
                                parse_quote! { #trait_ty_generics },
                            ),
                        }
                    };

                    // Replace `Self` segment to the new one
                    *seg = trait_segment;
                    continue;
                }

                match &mut seg.arguments {
                    PathArguments::None => { /* do nothing */ }
                    PathArguments::AngleBracketed(arg) => {
                        for arg in &mut arg.args {
                            match arg {
                                GenericArgument::Type(ty) => deselfify_type(ty, cx)?,
                                GenericArgument::AssocType(b) => deselfify_type(&mut b.ty, cx)?,
                                _ => { /* do nothing */ }
                            }
                        }
                    }
                    PathArguments::Parenthesized(arg) => {
                        for inp in &mut arg.inputs {
                            deselfify_type(inp, cx)?;
                        }

                        if let ReturnType::Type(_, ty) = &mut arg.output {
                            deselfify_type(&mut **ty, cx)?;
                        }
                    }
                }
            }
        }
        Type::Ptr(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::Reference(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::Slice(ty) => deselfify_type(&mut *ty.elem, cx)?,
        Type::TraitObject(ty) => {
            for p in &mut ty.bounds {
                if let TypeParamBound::Trait(tr) = p {
                    for seg in &mut tr.path.segments {
                        match &mut seg.arguments {
                            PathArguments::None => { /* do nothing */ }
                            PathArguments::AngleBracketed(arg) => {
                                for arg in &mut arg.args {
                                    match arg {
                                        GenericArgument::Type(ty) => deselfify_type(ty, cx)?,
                                        GenericArgument::AssocType(b) => {
                                            deselfify_type(&mut b.ty, cx)?
                                        }
                                        _ => { /* do nothing */ }
                                    }
                                }
                            }
                            PathArguments::Parenthesized(arg) => {
                                for inp in &mut arg.inputs {
                                    deselfify_type(inp, cx)?;
                                }

                                if let ReturnType::Type(_, ty) = &mut arg.output {
                                    deselfify_type(&mut **ty, cx)?;
                                }
                            }
                        }
                    }
                }
            }
        }
        Type::Tuple(ty) => {
            for ty in &mut ty.elems {
                deselfify_type(ty, cx)?;
            }
        }
        Type::Infer(_) => { /* do nothing */ }
        Type::Never(_) => { /* do nothing */ }
        Type::ImplTrait(ty) => {
            for p in &mut ty.bounds {
                if let TypeParamBound::Trait(tr) = p {
                    for seg in &mut tr.path.segments {
                        match &mut seg.arguments {
                            PathArguments::None => { /* do nothing */ }
                            PathArguments::AngleBracketed(arg) => {
                                for arg in &mut arg.args {
                                    match arg {
                                        GenericArgument::Type(ty) => deselfify_type(ty, cx)?,
                                        GenericArgument::AssocType(b) => {
                                            deselfify_type(&mut b.ty, cx)?
                                        }
                                        _ => { /* do nothing */ }
                                    }
                                }
                            }
                            PathArguments::Parenthesized(arg) => {
                                for inp in &mut arg.inputs {
                                    deselfify_type(inp, cx)?;
                                }

                                if let ReturnType::Type(_, ty) = &mut arg.output {
                                    deselfify_type(&mut **ty, cx)?;
                                }
                            }
                        }
                    }
                }
            }
        }
        x => {
            return Err(syn::Error::new(
                x.span(),
                "unsupported type for deselfifying",
            ))
        }
    }
    Ok(())
}
