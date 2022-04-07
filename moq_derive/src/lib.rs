mod context;
mod mock_action;
mod mock_struct;
mod symbols;
mod utils;

use crate::context::Context;
use crate::mock_action::make_action_def;
use crate::mock_action::make_action_impl;
use crate::mock_action::make_action_kind_def;
use crate::mock_struct::make_struct_def;
use crate::mock_struct::make_struct_impl;
use crate::mock_struct::make_struct_impl_drop;
use crate::mock_struct::make_struct_impl_trait;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, ItemEnum, ItemImpl, ItemStruct, ItemTrait};

/// Macro that provides mock struct generating that implements trait
///
/// ## Example
/// ```ignore
/// use moq::automock;
///
/// #[automock]
/// trait Trait {
///     fn func(&self, arg: i32) -> String;
/// }
///
/// #[test]
/// fn test_ok() {
///     let mock = TraitMock::new()
///         .expect_call_func(|arg: i64| {
///             assert_eq!(arg, 42);
///             format!("Hello, {}", arg)
///         })
///         .expect_call_func(|arg: i64| {
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
    let mock = parse_macro_input!(input as Mock);

    let output = quote! {
        #mock
    };
    output.into()
}

struct Mock {
    trait_def: ItemTrait,
    struct_def: ItemStruct,
    struct_impl: ItemImpl,
    struct_impl_trait: ItemImpl,
    struct_impl_drop: ItemImpl,
    action_def: ItemEnum,
    action_impl: ItemImpl,
    action_kind_def: ItemEnum,
}

impl Parse for Mock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let cx = Context::parse(input)?;

        Ok(Self {
            trait_def: cx.trait_def.clone(),
            struct_def: make_struct_def(&cx),
            struct_impl: make_struct_impl(&cx),
            struct_impl_trait: make_struct_impl_trait(&cx),
            struct_impl_drop: make_struct_impl_drop(&cx),
            action_def: make_action_def(&cx),
            action_impl: make_action_impl(&cx),
            action_kind_def: make_action_kind_def(&cx),
        })
    }
}

impl ToTokens for Mock {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let trait_def = &self.trait_def;
        let struct_def = &self.struct_def;
        let struct_impl = &self.struct_impl;
        let struct_impl_trait = &self.struct_impl_trait;
        let struct_impl_drop = &self.struct_impl_drop;
        let action_def = &self.action_def;
        let action_impl = &self.action_impl;
        let action_kind_def = &self.action_kind_def;

        let mock_tokens = quote! {
            #trait_def

            #struct_def
            #struct_impl
            #struct_impl_trait
            #struct_impl_drop

            #action_def
            #action_impl
            #action_kind_def
        };

        mock_tokens.to_tokens(tokens);
    }
}
