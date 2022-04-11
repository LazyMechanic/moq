extern crate core;

mod action;
mod context;
mod mock;
mod symbols;
mod utils;

use crate::action::{Action, ActionCollection};
use crate::context::Context;
use crate::mock::Mock;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, ItemTrait, TraitItem};

/// Macro that provides mock struct generating that implements trait
///
/// ## Example
/// ```
/// # use moq_derive::automock;
///
/// #[automock]
/// trait Trait {
///     fn func(&self, arg: i32) -> String;
/// }
///
/// #[test]
/// fn test_ok() {
///     let mock = TraitMock::new()
///         .expect_func(|arg: i64| {
///             assert_eq!(arg, 42);
///             format!("Hello, {}", arg)
///         })
///         .expect_func(|arg: i64| {
///             assert_eq!(arg, -1);
///             format!("Hello again, {}", -1)
///         });
///     
///     mock.func(42);
///     mock.func(-1);
/// }
/// ```
#[proc_macro_attribute]
pub fn automock(_args: TokenStream, input: TokenStream) -> TokenStream {
    let p = parse_macro_input!(input as Parser);
    let output = quote! { #p };
    output.into()
}

struct Parser {
    trait_def: ItemTrait,
    actions_def: ActionCollection,
    actions: Vec<Action>,
    mock: Mock,
}

impl Parse for Parser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cx = Context::parse(input)?;
        let actions_def = ActionCollection::parse(&cx)?;
        let actions = cx
            .trait_def
            .items
            .iter()
            .filter_map(|item| match item {
                TraitItem::Method(item) => Some(&item.sig),
                _ => None,
            })
            .map(|sig| Action::parse(&cx, sig))
            .collect::<Result<_, _>>()?;
        let mock = Mock::parse(&cx)?;

        Ok(Self {
            trait_def: cx.trait_def_orig,
            actions_def,
            actions,
            mock,
        })
    }
}

impl ToTokens for Parser {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.trait_def.to_tokens(tokens);
        self.actions_def.to_tokens(tokens);
        self.actions.iter().for_each(|act| act.to_tokens(tokens));
        self.mock.to_tokens(tokens);
    }
}
