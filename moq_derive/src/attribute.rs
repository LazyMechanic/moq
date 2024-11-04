use crate::symbols;
use crate::symbols::Symbol;
use if_chain::if_chain;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use std::collections::HashSet;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    parse_quote, Expr, ExprLit, GenericArgument, Lit, MetaNameValue, Path, PathArguments, Type,
};
use syn::{Ident, Meta};

#[derive(Debug, Default)]
pub struct AttributePresent<'a> {
    set: HashSet<Symbol<'a>>,
}

impl<'a> AttributePresent<'a> {
    pub fn check_and_hit<A>(&mut self, attr: &A) -> Result<(), syn::Error>
    where
        A: Spanned + Symboled,
    {
        let symbol = <A as Symboled>::symbol();
        self.check(&symbol, attr.span())?;
        self.hit(symbol);
        Ok(())
    }

    pub fn check(&self, symbol: &Symbol<'a>, span: Span) -> Result<(), syn::Error> {
        if self.set.contains(symbol) {
            Err(syn::Error::new(
                span,
                format_args!("duplicate moq attribute: `{symbol}`",),
            ))
        } else {
            Ok(())
        }
    }

    pub fn check_conflict(
        &self,
        maybe_present: &Symbol<'a>,
        new: &Symbol<'a>,
        span: Span,
    ) -> Result<(), syn::Error> {
        if self.set.contains(maybe_present) {
            Err(syn::Error::new(
                span,
                format_args!("conflict moq attributes: `{maybe_present}` and `{new}`",),
            ))
        } else {
            Ok(())
        }
    }

    pub fn hit(&mut self, symbol: Symbol<'a>) {
        self.set.insert(symbol);
    }
}

pub trait Symboled {
    fn symbol() -> Symbol<'static>;
}

#[derive(Debug, Clone)]
pub enum MoqAttribute {
    Default(DefaultAttribute),
    DefaultWith(DefaultWithAttribute),
    Output(OutputAttribute),
    Rename(RenameAttribute),
}

impl Symboled for MoqAttribute {
    fn symbol() -> Symbol<'static> {
        symbols::MOQ
    }
}

impl MoqAttribute {
    pub fn unsupported_error(&self) -> syn::Error {
        syn::Error::new_spanned(self, "unsupported attribute")
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

#[derive(Debug, Clone)]
pub struct DefaultAttribute {
    value: DefaultAttributeValue,
    raw: Meta,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefaultAttributeValue {
    Flag,
    Expr(Expr),
}

impl DefaultAttribute {
    pub fn value(&self) -> &DefaultAttributeValue {
        &self.value
    }

    pub fn into_value(self) -> DefaultAttributeValue {
        self.value
    }

    pub fn unsupported_error(&self) -> syn::Error {
        syn::Error::new_spanned(self, "unsupported attribute")
    }
}

impl Symboled for DefaultAttribute {
    fn symbol() -> Symbol<'static> {
        symbols::DEFAULT
    }
}

impl Parse for DefaultAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let raw = input.parse::<Meta>()?;
        if raw.path() != Self::symbol() {
            return Err(syn::Error::new_spanned(
                raw.path(),
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        match &raw {
            Meta::Path(_) => Ok(Self {
                value: DefaultAttributeValue::Flag,
                raw,
            }),
            Meta::NameValue(meta) => Ok(Self {
                value: DefaultAttributeValue::Expr(meta.value.clone()),
                raw,
            }),
            other => Err(syn::Error::new_spanned(other, "unsupported attribute")),
        }
    }
}

impl ToTokens for DefaultAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.raw.to_tokens(tokens);
    }
}

// ======================= DefaultWithAttribute ======================= //

#[derive(Debug, Clone)]
pub struct DefaultWithAttribute {
    value: Path,
    raw: MetaNameValue,
}

impl DefaultWithAttribute {
    pub fn value(&self) -> &Path {
        &self.value
    }

    pub fn into_value(self) -> Path {
        self.value
    }

    pub fn unsupported_error(&self) -> syn::Error {
        syn::Error::new_spanned(self, "unsupported attribute")
    }
}

impl Symboled for DefaultWithAttribute {
    fn symbol() -> Symbol<'static> {
        symbols::DEFAULT_WITH
    }
}

impl Parse for DefaultWithAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let raw = input.parse::<MetaNameValue>()?;

        if raw.path != Self::symbol() {
            return Err(syn::Error::new_spanned(
                &raw.path,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = &raw.value
        else {
            return Err(syn::Error::new_spanned(
                &raw.value,
                "expected string literal",
            ));
        };

        let path = lit.parse::<Path>()?;

        Ok(Self { value: path, raw })
    }
}

impl ToTokens for DefaultWithAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.raw.to_tokens(tokens);
    }
}

// ======================= OutputAttribute ======================= //

#[derive(Debug, Clone)]
pub struct OutputAttribute {
    value: OutputAttributeValue,
    raw: MetaNameValue,
}

#[derive(Debug, Clone)]
pub enum OutputAttributeValue {
    FullPath(Path),
    InferPath(Path),
}

impl OutputAttribute {
    pub fn value(&self) -> &OutputAttributeValue {
        &self.value
    }

    pub fn into_value(self) -> OutputAttributeValue {
        self.value
    }

    pub fn unsupported_error(&self) -> syn::Error {
        syn::Error::new_spanned(self, "unsupported attribute")
    }
}

impl Symboled for OutputAttribute {
    fn symbol() -> Symbol<'static> {
        symbols::OUTPUT
    }
}

impl Parse for OutputAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let raw = input.parse::<MetaNameValue>()?;
        if raw.path != Self::symbol() {
            return Err(syn::Error::new_spanned(
                &raw.path,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = &raw.value
        else {
            return Err(syn::Error::new_spanned(
                &raw.value,
                "expected string literal",
            ));
        };

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
                let value = OutputAttributeValue::InferPath(parse_quote! { #path });
                Ok(Self { value, raw })
            } else {
                // #[moq(return = "::path::to::FullType")]
                let value = OutputAttributeValue::FullPath(path);
                Ok(Self { value, raw })
            }
        }
    }
}

impl ToTokens for OutputAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.raw.to_tokens(tokens);
    }
}

// ======================= RenameAttribute ======================= //

#[derive(Debug, Clone)]
pub struct RenameAttribute {
    value: Ident,
    raw: MetaNameValue,
}

impl RenameAttribute {
    pub fn value(&self) -> &Ident {
        &self.value
    }

    pub fn into_value(self) -> Ident {
        self.value
    }

    pub fn unsupported_error(&self) -> syn::Error {
        syn::Error::new_spanned(self, "unsupported attribute")
    }
}

impl Symboled for RenameAttribute {
    fn symbol() -> Symbol<'static> {
        symbols::RENAME
    }
}

impl Parse for RenameAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let raw = input.parse::<MetaNameValue>()?;
        if raw.path != Self::symbol() {
            return Err(syn::Error::new_spanned(
                &raw.path,
                format_args!("'{}' expected", Self::symbol()),
            ));
        }

        let Expr::Lit(ExprLit {
            lit: Lit::Str(lit), ..
        }) = &raw.value
        else {
            return Err(syn::Error::new_spanned(
                &raw.value,
                "expected string literal",
            ));
        };

        let ident = lit.parse::<Ident>()?;

        Ok(Self { value: ident, raw })
    }
}

impl ToTokens for RenameAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.raw.to_tokens(tokens);
    }
}
