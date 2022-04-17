use crate::utils;

use crate::env::Env;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parenthesized, parse_quote, Block, FnArg, Generics, Ident, Pat, ReturnType, Token, Type,
};

pub struct Lambda {
    self_ident: Ident,
    self_generics: Generics,
    self_env: Env,

    trait_ty: Type,

    call_asyncness: Option<Token![async]>,
    call_args_pat: Vec<Pat>,
    call_args_ty: Vec<Type>,
    call_ret_ty: Type,
    call_block: Block,
}

impl Parse for Lambda {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse env
        let env = Env::parse(input)?;

        // Parse signature
        // Example: async fn <'a>(arg: &'a str) -> &'a str {}
        let asyncness: Option<Token![async]> = input.parse()?;
        let _fn_token: Token![fn] = input.parse()?;
        let mut generics: Generics = input.parse()?;

        let inputs = {
            let content;
            let _paren_token = parenthesized!(content in input);
            utils::parse_fn_args(&content)?
        };

        let output: ReturnType = input.parse()?;
        generics.where_clause = input.parse()?;
        let block: Block = input.parse()?;

        // Process parsed values
        let self_ident = format_ident!("__MoqLambda");
        let self_generics = generics;
        let self_env = env;

        let trait_ty = if asyncness.is_some() {
            parse_quote! { ::moq::AsyncFunc }
        } else {
            parse_quote! { ::moq::Func }
        };

        let call_asyncness = asyncness;
        let call_args_pat = inputs
            .iter()
            .filter_map(|inp| match inp {
                FnArg::Receiver(x) if self_env.is_empty() => Some(Err(syn::Error::new(
                    x.span(),
                    "'self' argument is unexpected",
                ))),
                FnArg::Receiver(_) => None,
                FnArg::Typed(inp) => Some(Ok((*inp.pat).clone())),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let call_args_ty = inputs
            .iter()
            .filter_map(|inp| match inp {
                FnArg::Receiver(x) if self_env.is_empty() => Some(Err(syn::Error::new(
                    x.span(),
                    "'self' argument is unexpected",
                ))),
                FnArg::Receiver(_) => None,
                FnArg::Typed(inp) => Some(Ok((*inp.ty).clone())),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let call_ret_ty: Type = match output {
            ReturnType::Default => {
                parse_quote! { () }
            }
            ReturnType::Type(_, ty) => *ty,
        };
        let call_block = block;

        Ok(Self {
            self_ident,
            self_generics,
            self_env,
            trait_ty,
            call_asyncness,
            call_args_pat,
            call_args_ty,
            call_ret_ty,
            call_block,
        })
    }
}

impl ToTokens for Lambda {
    fn to_tokens(&self, dst: &mut proc_macro2::TokenStream) {
        let self_ident = &self.self_ident;
        let (self_impl_generics, _self_ty_generics, self_where_clause) =
            self.self_generics.split_for_impl();
        let (self_env_fields_def, self_env_fields, self_env_args) = self.self_env.split();

        let trait_ty = &self.trait_ty;
        let trait_ty_generics = {
            let args_ty = &self.call_args_ty;
            let ret_ty = &self.call_ret_ty;
            quote! { <(#(#args_ty,)*), #ret_ty> }
        };

        let call_args_pat = &self.call_args_pat;
        let call_args_ty = &self.call_args_ty;
        let call_ret_ty = &self.call_ret_ty;
        let call_block = &self.call_block;
        let call_asyncness = &self.call_asyncness;
        let async_trait = if self.call_asyncness.is_some() {
            Some(quote! { #[::moq::async_trait] })
        } else {
            None
        };

        let self_def = quote! {
            struct #self_ident {
                #self_env_fields_def
            }
        };

        let self_impl = quote! {
            impl #self_ident {
                fn new(#self_env_fields_def) -> Self {
                    Self {
                        #self_env_fields
                    }
                }
            }
        };

        let trait_impl = quote! {
            #async_trait
            impl #self_impl_generics #trait_ty #trait_ty_generics for #self_ident #self_where_clause {
                #call_asyncness fn call<'__moq>(&'__moq self, (#(#call_args_pat,)*): (#(#call_args_ty,)*)) -> #call_ret_ty
                where
                    #(#call_args_ty : '__moq,)*
                    #call_ret_ty : '__moq,
                {
                    #call_block
                }
            }
        };

        let result = quote! {
            {
                #self_def
                #self_impl
                #trait_impl
                #self_ident::new(#self_env_args)
            }
        };

        result.to_tokens(dst);
    }
}
