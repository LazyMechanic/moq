use crate::symbols;

use quote::format_ident;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, BoundLifetimes, FnArg, GenericArgument, GenericParam, Generics, Ident, ItemTrait,
    Lifetime, Path, PathArguments, ReturnType, Signature, Token, TraitItem, Type, TypeParamBound,
    WhereClause, WherePredicate,
};

pub fn format_mock_ident(trait_ident: &Ident) -> Ident {
    format_ident!("{}Mock", trait_ident)
}

pub fn format_actions_ident(trait_ident: &Ident) -> Ident {
    let mock_ident = format_mock_ident(trait_ident);
    format_ident!("__{}_Actions", mock_ident)
}

pub fn format_action_ident(trait_ident: &Ident, func_ident: &Ident) -> Ident {
    let mock_ident = format_mock_ident(trait_ident);
    format_ident!("__{}_{}_Action", mock_ident, func_ident)
}

pub fn demoq_attr_trait_def(mut trait_def: ItemTrait) -> ItemTrait {
    trait_def.items = trait_def
        .items
        .into_iter()
        .map(|item| match item {
            TraitItem::Const(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Const(item)
            }
            TraitItem::Method(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Method(item)
            }
            TraitItem::Type(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Type(item)
            }
            TraitItem::Macro(mut item) => {
                item.attrs = item
                    .attrs
                    .into_iter()
                    .filter(|attr| attr.path != symbols::MOQ)
                    .collect();
                TraitItem::Macro(item)
            }
            item => item,
        })
        .collect();

    trait_def
}

/// Generate `Box<dyn Any>` type
pub fn boxed_any() -> Type {
    parse_quote! {
        ::std::boxed::Box<dyn ::std::any::Any
                            + ::std::marker::Send
                            + ::std::marker::Sync
                            + 'static>
    }
}

/// Generate `Box<dyn Func/AsyncFunc>` type
pub fn boxed_exp_func(func_sig: &Signature) -> Result<Type, syn::Error> {
    let bound = exp_func_trait_bound(func_sig)?;
    let ty = parse_quote! { ::std::boxed::Box<dyn #bound> };
    Ok(ty)
}

/// Generate `Func/AsyncFunc + Send + 'static` trait bound
pub fn exp_func_trait_bound(
    func_sig: &Signature,
) -> Result<Punctuated<TypeParamBound, Token![+]>, syn::Error> {
    let is_async = func_sig.asyncness.is_some();

    let lts = func_sig
        .generics
        .lifetimes()
        .map(|lt| lt.lifetime.clone())
        .collect();

    let args_ty = func_sig
        .inputs
        .iter()
        .filter_map(|inp| match inp {
            FnArg::Receiver(_) => None,
            FnArg::Typed(pt) => Some(&*pt.ty),
        })
        .cloned()
        .map(map_try_deanonymize_ty_lifetimes)
        .collect::<Result<Vec<_>, _>>()?;

    let ret_ty = match &func_sig.output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => Some(map_try_deanonymize_ty_lifetimes((**ty).clone())?),
    };

    Ok(func_trait_bound(is_async, lts, args_ty, ret_ty))
}

pub fn delifetiming_generics(gen: &Generics) -> Generics {
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
                x @ WherePredicate::Eq(_) => Some(x),
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

pub fn only_lifetimes_generics(gen: &Generics) -> Generics {
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

pub fn apply_static_bounds(generics: Generics) -> Generics {
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
//
// pub fn find_used_generic_types_in_sig(generics: &Generics, func_sig: &Signature) -> Vec<Ident> {
//     let generic_tys = generics
//         .params
//         .iter()
//         .filter_map(|p| match p {
//             GenericParam::Type(ty) => Some(&ty.ident),
//             _ => None,
//         })
//         .collect::<HashSet<_>>();
//
//     let mut res = HashSet::new();
//     for inp in &func_sig.inputs {
//         match inp {
//             FnArg::Receiver(_) => {}
//             FnArg::Typed(arg) => res.extend(find_used_generic_types_in_ty(&generic_tys, &*arg.ty)),
//         }
//     }
//     match &func_sig.output {
//         ReturnType::Type(_, ty) => res.extend(find_used_generic_types_in_ty(&generic_tys, &**ty)),
//         _ => {}
//     }
//
//     res.into_iter().collect()
// }
//
// fn find_used_generic_types_in_ty<'a>(
//     generic_tys: &'a HashSet<&'a Ident>,
//     ty: &'a Type,
// ) -> HashSet<Ident> {
//     let mut res = HashSet::new();
//
//     match ty {
//         Type::Array(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::BareFn(ty) => {
//             if let ReturnType::Type(_, ty) = &ty.output {
//                 res.extend(find_used_generic_types_in_ty(generic_tys, &*ty));
//             }
//             for inp_arg in &ty.inputs {
//                 res.extend(find_used_generic_types_in_ty(generic_tys, &inp_arg.ty));
//             }
//         }
//         Type::Group(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::Paren(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::Path(ty) => {
//             if let Some(qself) = &ty.qself {
//                 res.extend(find_used_generic_types_in_ty(generic_tys, &*qself.ty));
//             }
//
//             res.extend(find_used_generic_types_in_path(generic_tys, &ty.path));
//         }
//         Type::Ptr(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::Reference(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::Slice(ty) => res.extend(find_used_generic_types_in_ty(generic_tys, &*ty.elem)),
//         Type::TraitObject(ty) => {
//             for p in &ty.bounds {
//                 match p {
//                     TypeParamBound::Trait(tr) => {
//                         res.extend(find_used_generic_types_in_path(generic_tys, &tr.path));
//                     }
//                     _ => {}
//                 }
//             }
//         }
//         Type::Tuple(ty) => {
//             for ty in &ty.elems {
//                 res.extend(find_used_generic_types_in_ty(generic_tys, ty));
//             }
//         }
//         _ => { /* do nothing */ }
//     }
//
//     res
// }
//
// fn find_used_generic_types_in_path<'a>(
//     generic_tys: &'a HashSet<&'a Ident>,
//     path: &'a Path,
// ) -> HashSet<Ident> {
//     let mut res = HashSet::new();
//     for seg in &path.segments {
//         match &seg.arguments {
//             PathArguments::None => {
//                 if generic_tys.contains(&seg.ident) {
//                     res.insert(seg.ident.clone());
//                 }
//             }
//             PathArguments::AngleBracketed(args) => {
//                 for gen_arg in &args.args {
//                     match gen_arg {
//                         GenericArgument::Type(ty) => {
//                             res.extend(find_used_generic_types_in_ty(generic_tys, ty))
//                         }
//                         _ => {}
//                     }
//                 }
//             }
//             PathArguments::Parenthesized(args) => {
//                 for inp_ty in &args.inputs {
//                     res.extend(find_used_generic_types_in_ty(generic_tys, inp_ty));
//                 }
//                 match &args.output {
//                     ReturnType::Type(_, ty) => {
//                         res.extend(find_used_generic_types_in_ty(generic_tys, &**ty))
//                     }
//                     _ => {}
//                 }
//             }
//         }
//     }
//     res
// }

/// Replace lifetime identifier `'_` to `'static`
fn deanonymize_lifetime(lt: &mut Lifetime) {
    if lt.ident == "_" {
        lt.ident = format_ident!("static");
    }
}

/// Replace lifetime identifier `'_` to `'static` on `Path`
fn deanonymize_path_lifetimes(path: &mut Path) -> Result<(), syn::Error> {
    for seg in &mut path.segments {
        match &mut seg.arguments {
            PathArguments::None => {}
            PathArguments::AngleBracketed(arg) => {
                for gen_arg in &mut arg.args {
                    match gen_arg {
                        GenericArgument::Lifetime(lt) => deanonymize_lifetime(lt),
                        GenericArgument::Type(ty) => deanonymize_ty_lifetimes(ty)?,
                        _ => {}
                    }
                }
            }
            PathArguments::Parenthesized(arg) => {
                if let ReturnType::Type(_, ty) = &mut arg.output {
                    deanonymize_ty_lifetimes(&mut *ty)?;
                }

                for inp_ty in &mut arg.inputs {
                    deanonymize_ty_lifetimes(inp_ty)?;
                }
            }
        }
    }

    Ok(())
}

/// Replace lifetime identifier `'_` to `'static` on `Type`
fn deanonymize_ty_lifetimes(ty: &mut Type) -> Result<(), syn::Error> {
    match ty {
        Type::Array(ty) => deanonymize_ty_lifetimes(&mut *ty.elem)?,
        Type::BareFn(ty) => {
            if let ReturnType::Type(_, ty) = &mut ty.output {
                deanonymize_ty_lifetimes(&mut *ty)?;
            }
            for inp_arg in &mut ty.inputs {
                deanonymize_ty_lifetimes(&mut inp_arg.ty)?;
            }
        }
        Type::Group(ty) => deanonymize_ty_lifetimes(&mut *ty.elem)?,
        Type::Infer(_) => { /* there are no lifetimes */ }
        Type::Never(_) => { /* there are no lifetimes */ }
        Type::Paren(ty) => deanonymize_ty_lifetimes(&mut *ty.elem)?,
        Type::Path(ty) => {
            if let Some(qself) = &mut ty.qself {
                deanonymize_ty_lifetimes(&mut qself.ty)?;
            }

            deanonymize_path_lifetimes(&mut ty.path)?;
        }
        Type::Ptr(ty) => deanonymize_ty_lifetimes(&mut *ty.elem)?,
        Type::Reference(ty) => {
            deanonymize_ty_lifetimes(&mut *ty.elem)?;
            if let Some(lt) = &mut ty.lifetime {
                deanonymize_lifetime(lt);
            }
        }
        Type::Slice(ty) => deanonymize_ty_lifetimes(&mut *ty.elem)?,
        Type::TraitObject(ty) => {
            for p in &mut ty.bounds {
                match p {
                    TypeParamBound::Trait(tr) => deanonymize_path_lifetimes(&mut tr.path)?,
                    TypeParamBound::Lifetime(lt) => deanonymize_lifetime(lt),
                }
            }
        }
        Type::Tuple(ty) => {
            for ty in &mut ty.elems {
                deanonymize_ty_lifetimes(ty)?;
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

fn map_try_deanonymize_ty_lifetimes(mut ty: Type) -> Result<Type, syn::Error> {
    deanonymize_ty_lifetimes(&mut ty)?;
    Ok(ty)
}

fn func_trait_bound(
    is_async: bool,
    lts: Vec<Lifetime>,
    args_ty: Vec<Type>,
    ret_ty: Option<Type>,
) -> Punctuated<TypeParamBound, Token![+]> {
    let func_trait = func_trait(is_async);

    let lts_bounds: Option<BoundLifetimes> = if lts.is_empty() {
        None
    } else {
        Some(parse_quote! { for<#(#lts),*> })
    };

    let args_ty: Type = if args_ty.is_empty() {
        parse_quote! { () }
    } else {
        parse_quote! { (#(#args_ty,)*) }
    };

    let ret_ty = ret_ty.unwrap_or_else(|| parse_quote! { () });

    parse_quote! {
        #lts_bounds #func_trait<#args_ty, #ret_ty>
            + ::std::marker::Send
            + ::std::marker::Sync
            + 'static
    }
}

fn func_trait(is_async: bool) -> Type {
    if is_async {
        parse_quote! { ::moq::AsyncFunc }
    } else {
        parse_quote! { ::moq::Func }
    }
}
