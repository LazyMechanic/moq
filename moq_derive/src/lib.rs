mod action;
mod automock;
mod context;
mod mock;
mod symbols;
mod utils;

use automock::Automock;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

/// Macro that provides mock struct generating that implements trait
///
/// ## Example
/// ```ignore
/// #[moq::automock]
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
    let p = parse_macro_input!(input as Automock);
    let output = quote! { #p };
    output.into()
}
