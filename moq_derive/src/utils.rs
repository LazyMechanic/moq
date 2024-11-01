use crate::context::Context;
use crate::{symbols, utils};
use if_chain::if_chain;
use itertools::Itertools;
use quote::{format_ident, quote};
use std::borrow::Borrow;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, AngleBracketedGenericArguments, Attribute, BoundLifetimes, Expr, ExprLit, FnArg,
    GenericArgument, GenericParam, Generics, Ident, ImplItem, ItemTrait, Lifetime, Lit, Meta,
    MetaNameValue, Path, PathArguments, PathSegment, QSelf, ReturnType, Signature, Token,
    TraitItem, TraitItemFn, Type, TypeParamBound, WhereClause, WherePredicate,
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
        .map(|mut ty| {
            deanonymize_lifetimes_type(&mut ty)?;
            Ok::<_, syn::Error>(ty)
        })
        .map(|mut maybe_ty| {
            let mut ty = maybe_ty?;
            deselfify_type(cx, &mut ty)?;
            Ok::<_, syn::Error>(ty)
        })
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
                let moq_attrs_iter = moqify_attrs(trait_func.attrs.iter());
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
                deanonymize_lifetimes_type(&mut ret_ty)?;
                deselfify_type(cx, &mut ret_ty)?;
                Ok(Some(ret_ty))
            }
            other => {
                let mut ret_ty = other.clone();
                deanonymize_lifetimes_type(&mut ret_ty)?;
                deselfify_type(cx, &mut ret_ty)?;
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
                item.attrs = demoqify_attrs(item.attrs).collect();
                TraitItem::Const(item)
            }
            TraitItem::Fn(mut item) => {
                item.attrs = demoqify_attrs(item.attrs).collect();
                TraitItem::Fn(item)
            }
            TraitItem::Type(mut item) => {
                item.attrs = demoqify_attrs(item.attrs).collect();
                TraitItem::Type(item)
            }
            TraitItem::Macro(mut item) => {
                item.attrs = demoqify_attrs(item.attrs).collect();
                TraitItem::Macro(item)
            }
            item => item,
        })
        .collect();

    trait_def
}

pub fn moqify_attrs<I, A>(attrs: I) -> impl Iterator<Item = A>
where
    I: IntoIterator<Item = A>,
    A: Borrow<Attribute>,
{
    attrs
        .into_iter()
        .filter(|attr| attr.borrow().path() == symbols::MOQ)
}

pub fn demoqify_attrs<I, A>(attrs: I) -> impl Iterator<Item = A>
where
    I: IntoIterator<Item = A>,
    A: Borrow<Attribute>,
{
    attrs
        .into_iter()
        .filter(|attr| attr.borrow().path() != symbols::MOQ)
}

/// Removes lifetimes from generics
pub fn delifetimify_generics(gen: &Generics) -> Generics {
    let params = gen
        .params
        .iter()
        .filter(|&p| !matches!(p, GenericParam::Lifetime(_)))
        .cloned()
        .collect();

    let where_clause = gen.where_clause.clone().map(|mut wc| {
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

    Generics {
        lt_token: gen.lt_token,
        params,
        gt_token: gen.gt_token,
        where_clause,
    }
}

/// Filters generics for lifetimes and returns new generics with only lifetimes
pub fn lifetimify_generics(gen: &Generics) -> Generics {
    let params = gen
        .params
        .iter()
        .filter(|&p| matches!(p, GenericParam::Lifetime(_)))
        .cloned()
        .collect();

    let where_clause = gen.where_clause.clone().map(|mut wc| {
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

    Generics {
        lt_token: gen.lt_token,
        params,
        gt_token: gen.gt_token,
        where_clause,
    }
}

pub trait MergeGenerics {
    fn merge(self, other: Generics) -> Generics;
}

impl MergeGenerics for Generics {
    fn merge(self, other: Generics) -> Generics {
        merge_generics(self, other)
    }
}

/// Merge two generics into one
pub fn merge_generics(left: Generics, right: Generics) -> Generics {
    let params = left.params.into_iter().chain(right.params).collect();

    let where_clause = match (left.where_clause, right.where_clause) {
        (Some(trait_where_clause), Some(func_where_clause)) => {
            let mut res_where_clause = trait_where_clause;
            res_where_clause
                .predicates
                .extend(func_where_clause.predicates);
            Some(res_where_clause)
        }
        (Some(trait_where_clause), None) => Some(trait_where_clause),
        (None, Some(func_where_clause)) => Some(func_where_clause),
        (None, None) => None,
    };

    Generics {
        lt_token: Some(<Token![<]>::default()),
        params,
        gt_token: Some(<Token![>]>::default()),
        where_clause,
    }
}

/// Applies `T: 'static` bound on every generic parameter
pub fn staticize(generics: Generics) -> Generics {
    let idents = generics
        .params
        .iter()
        .filter_map(|p| match p {
            GenericParam::Type(ty) => Some(&ty.ident),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut where_clause = generics.where_clause.unwrap_or(WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::default(),
    });
    for ident in idents {
        let p = parse_quote! { #ident: 'static };
        where_clause.predicates.push(p);
    }

    Generics {
        lt_token: generics.lt_token,
        params: generics.params,
        gt_token: generics.gt_token,
        where_clause: Some(where_clause),
    }
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
                        deselfify_type(cx, &mut *arg.ty)?;
                    }
                }
            }

            if let ReturnType::Type(_, ty) = &mut f.sig.output {
                deselfify_type(cx, &mut **ty)?;
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

/// Turns `Self::AssocItem` -> `<TraitMock as Trait>::AssocItem`
pub fn deselfify_type(cx: &Context, ty: &mut Type) -> Result<(), syn::Error> {
    match ty {
        Type::Array(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::BareFn(ty) => {
            if let ReturnType::Type(_, ty) = &mut ty.output {
                deselfify_type(cx, &mut **ty)?;
            }
            for inp_arg in &mut ty.inputs {
                deselfify_type(cx, &mut inp_arg.ty)?;
            }
        }
        Type::Group(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::Paren(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::Path(ty) => {
            if let Some(qself) = &mut ty.qself {
                deselfify_type(cx, &mut *qself.ty)?
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
                                GenericArgument::Type(ty) => deselfify_type(cx, ty)?,
                                GenericArgument::AssocType(b) => deselfify_type(cx, &mut b.ty)?,
                                _ => { /* do nothing */ }
                            }
                        }
                    }
                    PathArguments::Parenthesized(arg) => {
                        for inp in &mut arg.inputs {
                            deselfify_type(cx, inp)?;
                        }

                        if let ReturnType::Type(_, ty) = &mut arg.output {
                            deselfify_type(cx, &mut **ty)?;
                        }
                    }
                }
            }
        }
        Type::Ptr(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::Reference(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::Slice(ty) => deselfify_type(cx, &mut *ty.elem)?,
        Type::TraitObject(ty) => {
            for p in &mut ty.bounds {
                if let TypeParamBound::Trait(tr) = p {
                    for seg in &mut tr.path.segments {
                        match &mut seg.arguments {
                            PathArguments::None => { /* do nothing */ }
                            PathArguments::AngleBracketed(arg) => {
                                for arg in &mut arg.args {
                                    match arg {
                                        GenericArgument::Type(ty) => deselfify_type(cx, ty)?,
                                        GenericArgument::AssocType(b) => {
                                            deselfify_type(cx, &mut b.ty)?
                                        }
                                        _ => { /* do nothing */ }
                                    }
                                }
                            }
                            PathArguments::Parenthesized(arg) => {
                                for inp in &mut arg.inputs {
                                    deselfify_type(cx, inp)?;
                                }

                                if let ReturnType::Type(_, ty) = &mut arg.output {
                                    deselfify_type(cx, &mut **ty)?;
                                }
                            }
                        }
                    }
                }
            }
        }
        Type::Tuple(ty) => {
            for ty in &mut ty.elems {
                deselfify_type(cx, ty)?;
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
                                        GenericArgument::Type(ty) => deselfify_type(cx, ty)?,
                                        GenericArgument::AssocType(b) => {
                                            deselfify_type(cx, &mut b.ty)?
                                        }
                                        _ => { /* do nothing */ }
                                    }
                                }
                            }
                            PathArguments::Parenthesized(arg) => {
                                for inp in &mut arg.inputs {
                                    deselfify_type(cx, inp)?;
                                }

                                if let ReturnType::Type(_, ty) = &mut arg.output {
                                    deselfify_type(cx, &mut **ty)?;
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

/// Turns lifetime identifier `'_` to `'static`
fn deanonymize_lifetime(lt: &mut Lifetime) {
    if lt.ident == "_" {
        lt.ident = format_ident!("static");
    }
}

/// Turns lifetime identifier `'_` to `'static` on `Path`
fn deanonymize_lifetimes_path(path: &mut Path) -> Result<(), syn::Error> {
    for seg in &mut path.segments {
        match &mut seg.arguments {
            PathArguments::None => {}
            PathArguments::AngleBracketed(arg) => {
                for gen_arg in &mut arg.args {
                    match gen_arg {
                        GenericArgument::Lifetime(lt) => deanonymize_lifetime(lt),
                        GenericArgument::Type(ty) => deanonymize_lifetimes_type(ty)?,
                        _ => {}
                    }
                }
            }
            PathArguments::Parenthesized(arg) => {
                if let ReturnType::Type(_, ty) = &mut arg.output {
                    deanonymize_lifetimes_type(&mut *ty)?;
                }

                for inp_ty in &mut arg.inputs {
                    deanonymize_lifetimes_type(inp_ty)?;
                }
            }
        }
    }

    Ok(())
}

/// Turns lifetime identifier `'_` to `'static` on `Type`
fn deanonymize_lifetimes_type(ty: &mut Type) -> Result<(), syn::Error> {
    match ty {
        Type::Array(ty) => deanonymize_lifetimes_type(&mut *ty.elem)?,
        Type::BareFn(ty) => {
            if let ReturnType::Type(_, ty) = &mut ty.output {
                deanonymize_lifetimes_type(&mut *ty)?;
            }
            for inp_arg in &mut ty.inputs {
                deanonymize_lifetimes_type(&mut inp_arg.ty)?;
            }
        }
        Type::Group(ty) => deanonymize_lifetimes_type(&mut *ty.elem)?,
        Type::Infer(_) => { /* there are no lifetimes */ }
        Type::Never(_) => { /* there are no lifetimes */ }
        Type::Paren(ty) => deanonymize_lifetimes_type(&mut *ty.elem)?,
        Type::Path(ty) => {
            if let Some(qself) = &mut ty.qself {
                deanonymize_lifetimes_type(&mut qself.ty)?;
            }

            deanonymize_lifetimes_path(&mut ty.path)?;
        }
        Type::Ptr(ty) => deanonymize_lifetimes_type(&mut *ty.elem)?,
        Type::Reference(ty) => {
            deanonymize_lifetimes_type(&mut *ty.elem)?;
            if let Some(lt) = &mut ty.lifetime {
                deanonymize_lifetime(lt);
            }
        }
        Type::Slice(ty) => deanonymize_lifetimes_type(&mut *ty.elem)?,
        Type::TraitObject(ty) => {
            for p in &mut ty.bounds {
                match p {
                    TypeParamBound::Trait(tr) => deanonymize_lifetimes_path(&mut tr.path)?,
                    TypeParamBound::Lifetime(lt) => deanonymize_lifetime(lt),
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            "unsupported type param bound",
                        ))
                    }
                }
            }
        }
        Type::Tuple(ty) => {
            for ty in &mut ty.elems {
                deanonymize_lifetimes_type(ty)?;
            }
        }
        Type::ImplTrait(ty) => {
            for p in &mut ty.bounds {
                match p {
                    TypeParamBound::Trait(tr) => deanonymize_lifetimes_path(&mut tr.path)?,
                    TypeParamBound::Lifetime(lt) => deanonymize_lifetime(lt),
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            "unsupported type param bound",
                        ))
                    }
                }
            }
        }
        x => {
            return Err(syn::Error::new(
                x.span(),
                "unsupported type for deanonymize",
            ))
        }
    }

    Ok(())
}
