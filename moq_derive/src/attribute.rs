use crate::symbols;
use crate::symbols::Symbol;
use if_chain::if_chain;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_quote, Expr, GenericArgument, LitStr, Path, PathArguments, Token, Type};
use syn::{Ident, Meta};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MoqAttribute {
    Default(DefaultAttribute),
    DefaultWith(DefaultWithAttribute),
    Output(OutputAttribute),
    Rename(RenameAttribute),
}

impl MoqAttribute {
    pub fn symbol(&self) -> Symbol<'static> {
        symbols::MOQ
    }
}

impl Parse for MoqAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ahead = input.fork();
        let meta = ahead.parse::<Meta>()?;

        if meta.path() == DefaultAttribute::symbol() {
            input.parse::<DefaultAttribute>().map(MoqAttribute::Default)
        } else if meta.path() == DefaultWithAttribute::symbol() {
            input
                .parse::<DefaultWithAttribute>()
                .map(MoqAttribute::DefaultWith)
        } else if meta.path() == OutputAttribute::symbol() {
            input.parse::<OutputAttribute>().map(MoqAttribute::Output)
        } else if meta.path() == RenameAttribute::symbol() {
            input.parse::<RenameAttribute>().map(MoqAttribute::Rename)
        } else {
            Err(syn::Error::new_spanned(meta, "unsupported attribute"))
        }
    }
}

impl ToTokens for MoqAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MoqAttribute::Default(inner) => inner.to_tokens(tokens),
            MoqAttribute::DefaultWith(inner) => inner.to_tokens(tokens),
            MoqAttribute::Output(inner) => inner.to_tokens(tokens),
            MoqAttribute::Rename(inner) => inner.to_tokens(tokens),
        }
    }
}

// ======================= DefaultAttribute ======================= //

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefaultAttribute {
    Flag,
    Path(Path),
    Expr(Expr),
}

impl DefaultAttribute {
    pub fn symbol() -> Symbol<'static> {
        symbols::DEFAULT
    }
}

impl Parse for DefaultAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let symbol = input.parse::<Ident>()?;
        if symbol != Self::symbol() {
            return Err(syn::Error::new_spanned(
                symbol,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        if input.peek(Token![=]) {
            let lit = input.parse::<LitStr>()?;
            if let Ok(path) = lit.parse::<Path>() {
                Ok(DefaultAttribute::Path(path))
            } else if let Ok(expr) = input.parse::<Expr>() {
                Ok(DefaultAttribute::Expr(expr))
            } else {
                Err(syn::Error::new_spanned(
                    lit,
                    "wrong value format, only path or expression allowed",
                ))
            }
        } else {
            Ok(DefaultAttribute::Flag)
        }
    }
}

impl ToTokens for DefaultAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            DefaultAttribute::Flag => {
                let symbol = format_ident!("{}", Self::symbol());

                let v = quote! { #symbol };
                v.to_tokens(tokens);
            }
            DefaultAttribute::Path(path) => {
                let symbol = format_ident!("{}", Self::symbol());
                let path_str = quote! { #path }.to_string();
                let lit = LitStr::new(&path_str, path.span());

                let v = quote! { #symbol = #lit };
                v.to_tokens(tokens);
            }
            DefaultAttribute::Expr(expr) => {
                let symbol = format_ident!("{}", Self::symbol());
                let expr_str = quote! { #expr }.to_string();
                let lit = LitStr::new(&expr_str, expr.span());

                let v = quote! { #symbol = #lit };
                v.to_tokens(tokens);
            }
        }
    }
}

// ======================= DefaultWithAttribute ======================= //

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DefaultWithAttribute {
    pub path: Path,
}

impl DefaultWithAttribute {
    pub fn symbol() -> Symbol<'static> {
        symbols::DEFAULT_WITH
    }
}

impl Parse for DefaultWithAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let symbol = input.parse::<Ident>()?;
        if symbol != Self::symbol() {
            return Err(syn::Error::new_spanned(
                symbol,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let _ = input.parse::<Token![=]>()?;
        let lit = input.parse::<LitStr>()?;
        let path = lit.parse::<Path>()?;

        Ok(DefaultWithAttribute { path })
    }
}

impl ToTokens for DefaultWithAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let symbol = format_ident!("{}", Self::symbol());
        let path = &self.path;
        let path_str = quote! { #path }.to_string();
        let lit = LitStr::new(&path_str, path.span());

        let v = quote! { #symbol = #lit };
        v.to_tokens(tokens);
    }
}

// ======================= OutputAttribute ======================= //

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OutputAttribute {
    FullPath(Path),
    InferPath(Path),
}

impl OutputAttribute {
    pub fn symbol() -> Symbol<'static> {
        symbols::OUTPUT
    }
}

impl Parse for OutputAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let symbol = input.parse::<Ident>()?;
        if symbol != Self::symbol() {
            return Err(syn::Error::new_spanned(
                symbol,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let _ = input.parse::<Token![=]>()?;
        let lit = input.parse::<LitStr>()?;
        let mut path = lit.parse::<Path>()?;

        let span = path.segments.span();
        let last_segment = path
            .segments
            .last_mut()
            .ok_or_else(|| syn::Error::new(span, "last segment not found"))?;

        if_chain! {
            if let PathArguments::AngleBracketed(args) = &last_segment.arguments;
            if args.args.len() == 1;
            if matches!(
                args.args.first(),
                Some(GenericArgument::Type(Type::Infer(_)))
            );
            then {
                // #[moq(return = "::path::to::InferType<_>")]
                last_segment.arguments = PathArguments::None;
                Ok(OutputAttribute::InferPath(parse_quote! { #path }))
            } else {
                // #[moq(return = "::path::to::FullType")]
                Ok(OutputAttribute::FullPath(path))
            }
        }
    }
}

impl ToTokens for OutputAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            OutputAttribute::FullPath(path) => {
                let symbol = format_ident!("{}", Self::symbol());
                let path_str = quote! { #path }.to_string();
                let lit = LitStr::new(&path_str, path.span());

                let v = quote! { #symbol = #lit };
                v.to_tokens(tokens);
            }
            OutputAttribute::InferPath(path) => {
                let symbol = format_ident!("{}", Self::symbol());
                let path_str = quote! { #path<_> }.to_string();
                let lit = LitStr::new(&path_str, path.span());

                let v = quote! { #symbol = #lit };
                v.to_tokens(tokens);
            }
        }
    }
}

// ======================= RenameAttribute ======================= //

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RenameAttribute {
    pub ident: Ident,
}

impl RenameAttribute {
    pub fn symbol() -> Symbol<'static> {
        symbols::RENAME
    }
}

impl Parse for RenameAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let symbol = input.parse::<Ident>()?;
        if symbol != Self::symbol() {
            return Err(syn::Error::new_spanned(
                symbol,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let _ = input.parse::<Token![=]>()?;
        let lit = input.parse::<LitStr>()?;
        let ident = lit.parse::<Ident>()?;

        Ok(RenameAttribute { ident })
    }
}

impl ToTokens for RenameAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let symbol = format_ident!("{}", Self::symbol());
        let lit = LitStr::new(&self.ident.to_string(), self.ident.span());

        let v = quote! { #symbol = #lit };
        v.to_tokens(tokens);
    }
}
