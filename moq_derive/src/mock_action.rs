use crate::{utils, Context};

use itertools::Itertools;
use quote::quote;
use syn::{
    parse_quote, FnArg, ImplItem, ImplItemMethod, ItemEnum, ItemImpl, ReturnType, Type, Variant,
};

pub fn make_action_def(cx: &Context) -> ItemEnum {
    let action_ident = &cx.action_ident;
    let action_variants = cx
        .trait_impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Method(item) => Some(action_variant(item)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let (_, ty_generics, _) = cx.action_generics.split_for_impl();

    parse_quote! {
        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[doc(hidden)]
        enum #action_ident #ty_generics {
            #(#action_variants,)*
            __NonExhaustive,
        }
    }
}

fn action_variant(item: &ImplItemMethod) -> Variant {
    let ident = &item.sig.ident;
    let ret_ty = match &item.sig.output {
        ReturnType::Default => parse_quote!(()),
        ReturnType::Type(_, ty) => utils::prepare_type((**ty).clone()),
    };
    let args_ty = item
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Receiver(_self_arg) => None,
            FnArg::Typed(arg) => Some(utils::prepare_type(*arg.ty.clone())),
        })
        .collect::<Vec<_>>();

    let lifetimes = args_ty
        .iter()
        .filter_map(|arg_ty| match arg_ty {
            Type::Reference(ty) => ty.lifetime.clone(),
            _ => None,
        })
        .unique()
        .collect::<Vec<_>>();

    let func_trait = if item.sig.asyncness.is_none() {
        quote! { for<#(#lifetimes),*> ::moq::Func<(#(#args_ty,)*), #ret_ty> }
    } else {
        quote! { for<#(#lifetimes),*> ::moq::AsyncFunc<(#(#args_ty,)*), #ret_ty> }
    };

    parse_quote! {
        #ident {
            func: ::std::boxed::Box<dyn #func_trait
                                      + ::std::marker::Send
                                      + ::std::marker::Sync
                                      + 'static>,
        }
    }
}

pub fn make_action_impl(cx: &Context) -> ItemImpl {
    let action_ident = &cx.action_ident;
    let action_kind_ident = &cx.action_kind_ident;
    let action_variants = cx
        .trait_impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Method(item) => Some(&item.sig.ident),
            _ => None,
        })
        .collect::<Vec<_>>();
    let (impl_generics, ty_generics, where_clause) = cx.action_generics.split_for_impl();

    parse_quote! {
        impl #impl_generics #action_ident #ty_generics #where_clause {
            fn kind(&self) -> #action_kind_ident {
                match self {
                    #(#action_ident::#action_variants { .. } => #action_kind_ident::#action_variants,)*
                    #action_ident::__NonExhaustive => #action_kind_ident::__NonExhaustive,
                }
            }
        }
    }
}

pub fn make_action_kind_def(cx: &Context) -> ItemEnum {
    let action_kind_ident = &cx.action_kind_ident;
    let action_variants = cx
        .trait_impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Method(item) => Some(&item.sig.ident),
            _ => None,
        })
        .collect::<Vec<_>>();

    parse_quote! {
        #[allow(non_camel_case_types)]
        #[doc(hidden)]
        #[derive(Debug)]
        enum #action_kind_ident {
            #(#action_variants,)*
            __NonExhaustive,
        }
    }
}
