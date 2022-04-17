mod env;
mod lambda;
mod utils;

use crate::lambda::Lambda;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

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
    output.into()
}
