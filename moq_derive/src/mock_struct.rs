use crate::utils::prepare_type;
use crate::Context;

use itertools::Itertools;
use quote::{format_ident, quote};
use syn::{
    parse_quote, FnArg, GenericParam, ImplItem, ImplItemMethod, ItemImpl, ItemStruct, ReturnType,
    Type,
};

pub fn make_struct_def(cx: &Context) -> ItemStruct {
    let vis = &cx.trait_vis;
    let struct_ident = &cx.struct_ident;
    let action_ident = &cx.action_ident;
    let phantom_data_fields = cx
        .struct_generics
        .params
        .iter()
        .filter_map(|p| match p {
            GenericParam::Type(p) => {
                let field_ident = format_ident!("__{}", p.ident);
                let ty_ident = &p.ident;
                Some(quote! { #field_ident : ::std::marker::PhantomData<fn() -> #ty_ident> })
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let (_, ty_generics, where_clause) = cx.struct_generics.split_for_impl();

    parse_quote! {
        #[allow(non_snake_case)]
        #vis struct #struct_ident #ty_generics #where_clause {
            actions: ::std::vec::Vec<#action_ident #ty_generics>,
            cur_action_idx: ::std::sync::atomic::AtomicUsize,
            #(#phantom_data_fields),*
        }
    }
}

pub fn make_struct_impl(cx: &Context) -> ItemImpl {
    let struct_ident = &cx.struct_ident;
    let funcs = cx
        .trait_impl_items
        .iter()
        .filter_map(|item| match item {
            ImplItem::Method(item) => Some(mocking_func(cx, item)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let phantom_data_fields = cx
        .struct_generics
        .params
        .iter()
        .filter_map(|p| match p {
            GenericParam::Type(p) => Some(format_ident!("__{}", p.ident)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let (impl_generics, ty_generics, where_clause) = cx.struct_generics.split_for_impl();

    parse_quote! {
        impl #impl_generics #struct_ident #ty_generics #where_clause {
            pub fn new() -> Self {
                Self {
                    actions: ::std::vec::Vec::new(),
                    cur_action_idx: ::std::sync::atomic::AtomicUsize::new(0),
                    #(#phantom_data_fields: ::std::default::Default::default()),*
                }
            }

            #(#funcs)*
        }
    }
}

fn mocking_func(cx: &Context, item: &ImplItemMethod) -> ImplItemMethod {
    let action_ident = &cx.action_ident;
    let mockable_func_ident = &item.sig.ident;
    let func_ident = format_ident!("expect_call_{}", item.sig.ident);
    let ret_ty = match &item.sig.output {
        ReturnType::Default => parse_quote!(()),
        ReturnType::Type(_, ty) => prepare_type((**ty).clone()),
    };
    let args_ty = item
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Receiver(_self_arg) => None,
            FnArg::Typed(arg) => Some(prepare_type(*arg.ty.clone())),
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
        pub fn #func_ident <__MoqFunc> (mut self, f: __MoqFunc) -> Self
        where __MoqFunc: #func_trait
                       + ::std::marker::Send
                       + ::std::marker::Sync
                       + 'static
        {
            let act = #action_ident::#mockable_func_ident { func: Box::new(f) };
            self.actions.push(act);
            self
        }
    }
}

pub fn make_struct_impl_trait(cx: &Context) -> ItemImpl {
    let attrs = &cx.trait_attrs;
    let struct_ident = &cx.struct_ident;
    let trait_ident = &cx.trait_ident;
    let unsafety = cx.trait_unsafety;
    let items = &cx.trait_impl_items;
    let (impl_generics, ty_generics, where_clause) = cx.trait_generics.split_for_impl();
    let (_, struct_ty_generics, _) = cx.struct_generics.split_for_impl();

    parse_quote! {
        #(#attrs)*
        #unsafety impl #impl_generics #trait_ident #ty_generics for #struct_ident #struct_ty_generics #where_clause {
            #(#items)*
        }
    }
}

pub fn make_struct_impl_drop(cx: &Context) -> ItemImpl {
    let struct_ident = &cx.struct_ident;
    let (impl_generics, ty_generics, where_clause) = cx.struct_generics.split_for_impl();

    parse_quote! {
        impl #impl_generics ::std::ops::Drop for #struct_ident #ty_generics #where_clause {
            fn drop(&mut self) {
                if !::std::thread::panicking() {
                    let idx = self.cur_action_idx.load(::std::sync::atomic::Ordering::SeqCst);
                    assert_eq!(
                        idx,
                        self.actions.len(),
                        "there are still {} actions",
                        self.actions.len() - idx,
                    );
                }
            }
        }
    }
}
