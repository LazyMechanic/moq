use crate::context::Context;
use crate::utils;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, FnArg, Generics, Ident, ItemImpl, ItemStruct, Pat, ReturnType, Signature, Token,
    Type, TypeParamBound,
};

pub struct Action {
    self_ident: Ident,
    self_generics: Generics,
    func_trait_bound: Punctuated<TypeParamBound, Token![+]>,
    func_boxed_ty: Type,
    call_asyncness: Option<Token![async]>,
    call_awaiting: Option<(Token![.], Token![await])>,
    call_generics: Generics,
    call_args: Punctuated<FnArg, Token![,]>,
    call_ret: ReturnType,
    call_inner_args: Vec<Ident>,
    phantom: Option<(Ident, Type)>,
}

impl Action {
    pub fn parse(cx: &Context, func_sig: &Signature) -> Result<Self, syn::Error> {
        let self_ident = utils::format_action_ident(&cx.trait_def.ident, &func_sig.ident);
        let self_generics = {
            let trait_gen = utils::delifetimify_generics(&cx.trait_def.generics);
            let func_gen = utils::delifetimify_generics(&func_sig.generics);
            let merged = utils::merge_generics(trait_gen, func_gen);
            utils::staticize(merged)
        };

        let func_trait_bound = utils::make_exp_func_trait_bound(func_sig)?;
        let func_boxed_ty = utils::make_boxed_exp_func(func_sig)?;

        let call_asyncness = func_sig.asyncness;
        let call_awaiting = if func_sig.asyncness.is_some() {
            Some((Default::default(), Default::default()))
        } else {
            None
        };
        let call_generics = utils::lifetimify_generics(&func_sig.generics);
        let call_args = func_sig
            .inputs
            .clone()
            .into_iter()
            .filter(|inp| matches!(inp, &FnArg::Typed(_)))
            .collect();
        let call_ret = func_sig.output.clone();
        let call_inner_args = func_sig
            .inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Receiver(_) => None,
                FnArg::Typed(arg) => Some(arg),
            })
            .map(|arg| match &*arg.pat {
                Pat::Ident(pat) => Ok(pat.ident.clone()),
                x => Err(syn::Error::new(
                    x.span(),
                    "unsupported function argument pattern",
                )),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let phantom = {
            let generics_ty = self_generics
                .type_params()
                .map(|p| &p.ident)
                .collect::<Vec<_>>();
            if generics_ty.is_empty() {
                None
            } else {
                Some((
                    parse_quote! { _phantom },
                    parse_quote! { ::std::marker::PhantomData<fn() -> (#(#generics_ty,)*)> },
                ))
            }
        };

        Ok(Self {
            self_ident,
            self_generics,
            func_trait_bound,
            func_boxed_ty,
            call_asyncness,
            call_awaiting,
            call_generics,
            call_args,
            call_ret,
            call_inner_args,
            phantom,
        })
    }
}

impl ToTokens for Action {
    fn to_tokens(&self, dst: &mut TokenStream) {
        let self_ident = &self.self_ident;
        let (self_impl_generics, self_ty_generics, self_where_clause) =
            self.self_generics.split_for_impl();
        let func_trait_bound = &self.func_trait_bound;
        let func_boxed_ty = &self.func_boxed_ty;
        let call_asyncness = &self.call_asyncness;
        let call_awaiting = self
            .call_awaiting
            .as_ref()
            .map(|(_dot, _await)| quote! { .await });
        let (_call_impl_generics, call_ty_generics, call_where_clause) =
            self.call_generics.split_for_impl();
        let call_args = &self.call_args;
        let call_ret = &self.call_ret;
        let call_inner_args = &self.call_inner_args;
        let (phantom_def, phantom_init) = match &self.phantom {
            None => (None, None),
            Some((ident, ty)) => (
                Some(quote! { #ident: #ty, }),
                Some(quote! { #ident: ::std::default::Default::default(), }),
            ),
        };

        let def: ItemStruct = parse_quote! {
            #[allow(non_camel_case_types)]
            struct #self_ident #self_ty_generics #self_where_clause {
                func: #func_boxed_ty,
                #phantom_def
            }
        };

        let self_impl: ItemImpl = parse_quote! {
            impl #self_impl_generics #self_ident #self_ty_generics #self_where_clause {
                fn new<__MoqFunc>(f: __MoqFunc) -> Self
                where
                    __MoqFunc: #func_trait_bound,
                {
                    Self {
                        func: Box::new(f),
                        #phantom_init
                    }
                }

                #call_asyncness fn call #call_ty_generics (&self, #call_args) #call_ret #call_where_clause {
                    self.func
                        .call((#(#call_inner_args,)*))
                        #call_awaiting
                }
            }
        };

        def.to_tokens(dst);
        self_impl.to_tokens(dst);
    }
}

pub struct ActionCollection {
    pub ident: Ident,
}

impl ActionCollection {
    pub fn parse(cx: &Context) -> Result<Self, syn::Error> {
        let ident = utils::format_action_collection_ident(&cx.trait_def.ident);
        Ok(Self { ident })
    }
}

impl ToTokens for ActionCollection {
    fn to_tokens(&self, dst: &mut TokenStream) {
        let ident = &self.ident;
        let any_ty = utils::make_boxed_any();

        let def: ItemStruct = parse_quote! {
            #[allow(non_camel_case_types)]
            #[derive(Default)]
            struct #ident {
                cur_idx: ::std::sync::atomic::AtomicUsize,
                actions: ::std::vec::Vec<#any_ty>,
            }
        };

        let self_impl: ItemImpl = parse_quote! {
            impl #ident {
                fn add<__MoqAction>(&mut self, act: __MoqAction)
                where
                    __MoqAction: ::std::marker::Send
                               + ::std::marker::Sync
                               + 'static,
                {
                    let act = Box::new(act);
                    self.actions.push(act);
                }

                fn next<__MoqAction>(&self) -> &__MoqAction
                where
                    __MoqAction: ::std::marker::Send
                               + ::std::marker::Sync
                               + 'static,
                {
                    let idx = self.cur_idx.fetch_add(1, ::std::sync::atomic::Ordering::SeqCst);
                    assert!(
                        idx < self.actions.len(),
                        "an attempt to execute an extra action",
                    );

                    &*self.actions[idx].downcast_ref::<__MoqAction>().expect("downcasting action type failed, unexpected action")
                }
            }
        };

        let drop_impl: ItemImpl = parse_quote! {
            impl ::std::ops::Drop for #ident {
                fn drop(&mut self) {
                    if !::std::thread::panicking() {
                        let idx = self.cur_idx.load(::std::sync::atomic::Ordering::SeqCst);
                        assert_eq!(
                            idx,
                            self.actions.len(),
                            "there are still {} actions",
                            self.actions.len() - idx,
                        );
                    }
                }
            }
        };

        def.to_tokens(dst);
        self_impl.to_tokens(dst);
        drop_impl.to_tokens(dst);
    }
}
