use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parenthesized, parse_macro_input, parse_quote, Attribute, Block, FnArg, Generics, Ident, Pat,
    ReturnType, Token, Type,
};

/// Helpful macro for generating closure with specified lifetimes.
/// In rust you can't specify lifetimes for closure but you should, because lifetime
/// inference in closure is dumb. Then this macro will help.
/// Currently, capturing external variables is not possible, but this is in the plans.
///
/// Syntax is like anonymous function:
/// ```ignore
/// moq::lambda!(
/// //  asyncness
/// //  |        
/// //  |        generic params (lifetimes)
/// //  |        |
/// //  |        |    arguments
/// //  |        |    |
/// //  |        |    |             return type
/// //  |        |    |             |
/// //  vvvvv    vvvv vvvvvvvvvvvv  vvvvvvvvvvvvvvv
///     async fn <'a>(arg: &'a str) -> &'static str {
///         // ...
///     }
/// )
/// ```
#[proc_macro]
pub fn lambda(input: TokenStream) -> TokenStream {
    let p = parse_macro_input!(input as Lambda);
    let output = quote! { #p };
    //panic!("{}", output.to_string());
    output.into()
}

struct Lambda {
    self_ident: Ident,
    self_generics: Generics,
    // TODO: add captured fields
    trait_ty: Type,

    call_asyncness: Option<Token![async]>,
    call_args_pat: Vec<Pat>,
    call_args_ty: Vec<Type>,
    call_ret_ty: Type,
    call_block: Block,
}

impl Parse for Lambda {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // TODO: parse capturing like c++ lambda `[obj, &obj, &mut obj]`

        // Parse signature
        // Example: async fn <'a>(arg: &'a str) -> &'a str {}
        let asyncness: Option<Token![async]> = input.parse()?;
        let _fn_token: Token![fn] = input.parse()?;
        let mut generics: Generics = input.parse()?;

        let content;
        let _paren_token = parenthesized!(content in input);
        let inputs = parse_fn_args(&content)?;

        let output: ReturnType = input.parse()?;
        generics.where_clause = input.parse()?;
        let block: Block = input.parse()?;

        // Process parsed values
        let self_ident = format_ident!("__MoqLambda");
        let self_generics = generics;

        let trait_ty = if asyncness.is_some() {
            parse_quote! { ::moq::AsyncFunc }
        } else {
            parse_quote! { ::moq::Func }
        };

        let call_asyncness = asyncness;
        let call_args_pat = inputs
            .iter()
            .map(|inp| match inp {
                FnArg::Receiver(x) => {
                    Err(syn::Error::new(x.span(), "'self' argument is unexpected"))
                }
                FnArg::Typed(inp) => Ok((*inp.pat).clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let call_args_ty = inputs
            .iter()
            .map(|inp| match inp {
                FnArg::Receiver(x) => {
                    Err(syn::Error::new(x.span(), "'self' argument is unexpected"))
                }
                FnArg::Typed(inp) => Ok((*inp.ty).clone()),
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
                // TODO: add captured variables
            }
        };

        let self_impl = quote! {
            impl #self_ident {
                // TODO: pass captured variables
                fn new() -> Self {
                    Self{}
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
                // TODO: pass captured variables
                #self_ident::new()
            }
        };

        result.to_tokens(dst);
    }
}

fn parse_fn_args(input: ParseStream) -> Result<Punctuated<FnArg, Token![,]>, syn::Error> {
    let mut args = Punctuated::new();
    let mut has_receiver = false;

    while !input.is_empty() {
        let attrs = input.call(Attribute::parse_outer)?;

        let mut arg: FnArg = input.parse()?;
        match &mut arg {
            FnArg::Receiver(receiver) if has_receiver => {
                return Err(syn::Error::new(
                    receiver.self_token.span,
                    "unexpected second method receiver",
                ));
            }
            FnArg::Receiver(receiver) if !args.is_empty() => {
                return Err(syn::Error::new(
                    receiver.self_token.span,
                    "unexpected method receiver",
                ));
            }
            FnArg::Receiver(receiver) => {
                has_receiver = true;
                receiver.attrs = attrs;
            }
            FnArg::Typed(arg) => arg.attrs = attrs,
        }
        args.push_value(arg);

        if input.is_empty() {
            break;
        }

        let comma: Token![,] = input.parse()?;
        args.push_punct(comma);
    }

    Ok(args)
}
