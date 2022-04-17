use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::Bracket;
use syn::{bracketed, Expr, Field, Ident, Member, Token, Type, Visibility};

pub struct Env {
    args: Vec<EnvArg>,
}

struct EnvArg {
    expr: Expr,
    ident: Ident,
    ty: Type,
}

impl Env {
    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    pub fn split(&self) -> (EnvFieldsDef, EnvFields, EnvArgs) {
        (EnvFieldsDef(self), EnvFields(self), EnvArgs(self))
    }
}

impl Parse for Env {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args = if input.peek(Bracket) {
            let content;
            let _brackets_token = bracketed!(content in input);

            let mut args = Vec::new();
            loop {
                let expr: Expr = content.parse()?;

                let (expr, ident) = match expr {
                    // Type
                    // ---------------------vvvvvvvv
                    // arg                  as alias => i32
                    // arg.field            as alias => i32
                    // arg.field  as field2 as alias => i32
                    // arg.func() as field3 as alias => i32
                    Expr::Cast(x) => {
                        let expr = *x.expr;
                        let ident = match *x.ty {
                            Type::Path(ty) => {
                                if ty.qself.is_some() {
                                    return Err(syn::Error::new(
                                        ty.span(),
                                        "qself is unexpected, alias should be a single ident",
                                    ));
                                }

                                if ty.path.leading_colon.is_some() {
                                    return Err(syn::Error::new(
                                        ty.span(),
                                        "leading colon is unexpected, alias should be a single ident",
                                    ));
                                }

                                if ty.path.segments.len() != 1 {
                                    return Err(syn::Error::new(
                                        ty.span(),
                                        "multiple segments is unexpected, alias should be a single ident",
                                    ));
                                }

                                ty.path.segments[0].ident.clone()
                            }
                            x => {
                                return Err(syn::Error::new_spanned(
                                    x,
                                    "alias should be a single ident",
                                ))
                            }
                        };
                        (expr, ident)
                    }
                    // Single ident
                    // vvv
                    // arg => u32
                    Expr::Path(x) => {
                        if x.qself.is_some() {
                            return Err(syn::Error::new(
                                x.span(),
                                "qself is unexpected, alias should be a single ident",
                            ));
                        }

                        if x.path.leading_colon.is_some() {
                            return Err(syn::Error::new(
                                x.span(),
                                "leading colon is unexpected, alias should be a single ident",
                            ));
                        }

                        if x.path.segments.len() != 1 {
                            return Err(syn::Error::new(
                                x.span(),
                                "multiple segments is unexpected, alias should be a single ident",
                            ));
                        }

                        let ident = x.path.segments[0].ident.clone();
                        let expr = Expr::Path(x);
                        (expr, ident)
                    }
                    // Struct field
                    // ----vvvvv
                    // arg.field => u32
                    Expr::Field(x) => {
                        let ident = match &x.member {
                            Member::Named(ident) => {ident.clone()}
                            Member::Unnamed(_) => {return Err(syn::Error::new(x.span(), "only single ident and field of struct can be without alias, other types of expressions should use alias"))}
                        };
                        let expr = Expr::Field(x);
                        (expr, ident)
                    }
                    x => return Err(syn::Error::new_spanned(x, "only single ident and field of struct can be without alias, other types of expressions should use alias")),
                };

                let _arrow_token: Token![=>] = content.parse()?;
                let comma_span = content.span();
                let ty: Type = content.parse()?;
                let comma: Option<Token![,]> = content.parse()?;

                args.push(EnvArg { expr, ident, ty });

                if content.is_empty() {
                    break;
                }

                // Missing comma
                if comma.is_none() && !content.is_empty() {
                    return Err(syn::Error::new(comma_span, "missing comma"));
                }
            }

            args
        } else {
            Vec::new()
        };

        Ok(Self { args })
    }
}

// arg1: String, arg2: &'static str,
pub struct EnvFieldsDef<'a>(&'a Env);

impl ToTokens for EnvFieldsDef<'_> {
    fn to_tokens(&self, dst: &mut TokenStream) {
        let fields = self
            .0
            .args
            .iter()
            .map(|arg| Field {
                attrs: vec![],
                vis: Visibility::Inherited,
                ident: Some(arg.ident.clone()),
                colon_token: Some(<Token![:]>::default()),
                ty: arg.ty.clone(),
            })
            .collect::<Vec<_>>();

        let tok = quote! {
            #(#fields),*
        };

        tok.to_tokens(dst);
    }
}

// arg1, arg2
pub struct EnvFields<'a>(&'a Env);

impl ToTokens for EnvFields<'_> {
    fn to_tokens(&self, dst: &mut TokenStream) {
        let idents = self
            .0
            .args
            .iter()
            .map(|arg| arg.ident.clone())
            .collect::<Vec<_>>();

        let tok = quote! {
            #(#idents),*
        };

        tok.to_tokens(dst);
    }
}

// arg1, arg2.field
pub struct EnvArgs<'a>(&'a Env);

impl ToTokens for EnvArgs<'_> {
    fn to_tokens(&self, dst: &mut TokenStream) {
        if self.0.args.is_empty() {
            return;
        }

        let args = self
            .0
            .args
            .iter()
            .map(|arg| arg.expr.clone())
            .collect::<Vec<_>>();

        let tok = quote! {
            #(#args),*
        };

        tok.to_tokens(dst);
    }
}
