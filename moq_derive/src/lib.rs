mod action;
mod attribute;
mod automock_macro;
mod context;
mod mock;
mod symbols;
mod utils;
mod validate;

use proc_macro::TokenStream;

use crate::automock_macro::automock_impl;

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
///     let mock = MockTrait::new()
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
pub fn automock(args: TokenStream, input: TokenStream) -> TokenStream {
    match automock_impl(args.into(), input.into()) {
        Ok(tokens) => tokens.into(),
        Err(diag) => diag.emit_as_item_tokens().into(),
    }
}
