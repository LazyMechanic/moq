mod env;
mod lambda;
mod utils;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

use crate::lambda::Lambda;

/// Helpful macro for generating closure with specified lifetimes.
///
/// In rust, you can't specify lifetimes for closure, but you should, because lifetime
/// inference in closure is dumb. Then this macro will help.
///
/// Syntax is like anonymous function:
/// ```ignore
/// # fn main() {
/// #[derive(Debug, PartialEq)]
/// struct StructNamed {
///    field: i32,
/// }
///
/// impl StructNamed {
/// fn get_field(&self) -> i32 {
///        self.field
///    }
/// }
///
/// struct StructUnnamed(i32);
///
/// let arg1 = 1i32;
/// let arg2 = 2i32;
/// let arg3 = StructNamed { field: 3i32 };
/// let arg4 = StructNamed { field: 4i32 };
/// let arg5 = StructNamed { field: 5i32 };
/// let arg6 = StructNamed { field: 6i32 };
/// let arg7 = StructUnnamed(7i32);
///
/// lambda!(
/// // Environment capturing (optional)
/// //
/// //      expression          var alias expression type
/// //      |                   |         |
/// //      vvvvvvvvvvvvvvvv    vvvvvv    vvvvvvvvvvv
///     [
///         arg1                       => i32,
///         arg2             as field0 => i32,
///         arg3                       => StructNamed,
///         arg4.field                 => i32,
///         arg5.field       as field2 => i32,
///         arg6.get_field() as field3 => i32,
///         arg7.0           as field4 => i32
///     ]
///
/// // Function signature
/// //
/// //  asyncness
/// //  |        generic params (lifetimes)
/// //  |        |    object that captured env, needs only if env specified
/// //  |        |    |      arguments
/// //  |        |    |      |                return type
/// //  |        |    |      |                |
/// //  vvvvv    vvvv vvvvv  vvvvvvvvvvvv     vvvvvvvvvvvv
///     async fn <'a>(&self, arg: &'a str) -> &'static str {
///         assert_eq!(self.arg1, 1);
///         assert_eq!(self.field0, 2);
///         assert_eq!(self.arg3, StructNamed { field: 3 });
///         assert_eq!(self.field, 4);
///         assert_eq!(self.field2, 5);
///         assert_eq!(self.field3, 6);
///         assert_eq!(self.field4, 7);
///         // ...
///         # unimplemented!()
///     }
/// )
/// # ;
/// # }
/// ```
///
/// It turns to something like that:
/// ```ignore
/// # fn main() {
/// # struct StructNamed {
/// #    field: i32,
/// # }
/// # impl StructNamed {
/// # fn get_field(&self) -> i32 {
/// #        self.field
/// #    }
/// # }
/// # struct StructUnnamed(i32);
/// # let arg1 = 1i32;
/// # let arg2 = 2i32;
/// # let arg3 = StructNamed { field: 3i32 };
/// # let arg4 = StructNamed { field: 4i32 };
/// # let arg5 = StructNamed { field: 5i32 };
/// # let arg6 = StructNamed { field: 6i32 };
/// # let arg7 = StructUnnamed(7i32);
/// {
///     struct __MoqLambda {
///         arg1: i32,
///         field0: i32,
///         arg3: StructNamed,
///         field: i32,
///         field2: i32,
///         field3: i32,
///         field4: i32,
///     }
///     impl __MoqLambda {
///         fn new(
///             arg1: i32,
///             field0: i32,
///             arg3: StructNamed,
///             field: i32,
///             field2: i32,
///             field3: i32,
///             field4: i32,
///         ) -> Self {
///             Self {
///                 arg1,
///                 field0,
///                 arg3,
///                 field,
///                 field2,
///                 field3,
///                 field4,
///             }
///         }
///     }
///     #[::moq::async_trait]
///     impl<'a> ::moq::AsyncFunc<(&'a str,), &'static str> for __MoqLambda {
///         async fn call<'__moq>(&'__moq self, (arg,): (&'a str,)) -> &'static str
///         where (&'a str,): '__moq,
///               &'static str: '__moq
///         {
///             // ...
///             # unimplemented!()
///         }
///     }
///     __MoqLambda::new(
///         arg1,
///         arg2,
///         arg3,
///         arg4.field,
///         arg5.field,
///         arg6.get_field(),
///         arg7.0,
///     )
/// }
/// # ;
/// # }
/// ```
#[proc_macro]
pub fn lambda(input: TokenStream) -> TokenStream {
    let p = parse_macro_input!(input as Lambda);
    let output = quote! { #p };
    output.into()
}
