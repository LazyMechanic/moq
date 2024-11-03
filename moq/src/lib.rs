//! # Moq
//!
//! This library provides macro that generates mock struct that implements trait.
//!
//! To get started, just add an attribute macro to the trait for which you want to generate a mock object:
//! ```
//! #[moq::automock] // <-- this macro
//! trait TraitForMocking {
//!     fn func_that_i_want_to_test(&self, arg: String) -> Result<(), std::io::Error>;
//! }
//! ```
//!
//! Next, you can create a mock object and specify the expected behavior:
//! ```
//! # #[moq::automock]
//! # trait TraitForMocking {
//! #    fn func_that_i_want_to_test(&self, arg: String) -> Result<(), std::io::Error>;
//! # }
//! #[test]
//! fn some_test() {
//!     let mock = TraitForMockingMock::new()
//!         .expect_func_that_i_want_to_test(|arg: String| {
//!             assert_eq!(arg, "Hello, World!");
//!             Ok(())
//!         });
//!
//!     assert!(mock.func_that_i_want_to_test("Hello, World!".to_owned()).is_ok());
//! }
//! ```
//!
//! Also supports `async_trait`:
//! ```
//! #[moq::automock] // <- the main limitation is to specify
//!                  // the `automock` macro above the `async_trait`
//! #[async_trait::async_trait]
//! trait TraitForMocking {
//!    async fn func_that_i_want_to_test(&self, arg: String) -> Result<(), std::io::Error>;
//! }
//!
//! #[tokio::test]
//! async fn some_test() {
//!     let mock = TraitForMockingMock::new()
//!         .expect_func_that_i_want_to_test(|arg: String| async {
//!             assert_eq!(arg, "Hello, World!");
//!             Ok(())
//!         });
//!
//!     assert!(mock.func_that_i_want_to_test("Hello, World!".to_owned()).await.is_ok());
//! }
//! ```
//!
//! If function has arguments with lifetimes then `moq::lambda!` macro can help with it:
//! ```
//! #[derive(Debug, Eq, PartialEq)]
//! struct Struct<'a>(&'a str);
//!
//! #[moq::automock]
//! #[async_trait::async_trait]
//! trait TraitForMocking {
//!    async fn func_that_i_want_to_test<'a, 'b>(
//!         &self,
//!         arg1: Struct<'a>,
//!         arg2: Struct<'b>,
//!    ) -> Struct<'b>;
//! }
//!
//! #[tokio::test]
//! async fn some_test() {
//!     let mock = TraitForMockingMock::new()
//!         .expect_func_that_i_want_to_test(moq::lambda!(
//!             async fn <'a, 'b>(arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b> {
//!                 assert_eq!(arg1, Struct("Hello"));
//!                 assert_eq!(arg2, Struct("World"));
//!                 arg2
//!             }
//!         ));
//!
//!     assert_eq!(
//!         mock.func_that_i_want_to_test(
//!             Struct("Hello"),
//!             Struct("World"),
//!         ).await,
//!         Struct("World"),
//!     );
//! }
//! ```
//! More info about `moq::lambda!` macro you will find below.
//!
//! ## How its work
//! Generated mock struct checks that all specified expectation will calls in specified order.
//! If `f1()` is called while `f2()` is expected, then there will be a panic.
//! ```should_panic
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!     fn f2(&self);
//! }
//! let mock = MockTrait::new().expect_f2(||{});
//! mock.f1() // Panic
//! ```
//!
//! Checks if all expected functions are being called, if not then will be a panic.
//! ```should_panic
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!     fn f2(&self);
//! }
//! let mock = MockTrait::new()
//!     .expect_f1(||{})
//!     .expect_f2(||{});
//! mock.f1()
//! // Panic when mock will drop
//! ```
//!
//! ## Attribute `#[moq(default)]` function
//! Specifies that needs to use the default implementation of the function:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!     
//!     #[moq(default)]
//!     fn def(&self) {
//!         self.f1();
//!     }
//! }
//! let mock = MockTrait::new().expect_f1(|| {});
//! mock.def(); // Success
//! ```
//!
//! ## Attribute `#[moq(default = "...")]` associative type
//! Sets the associative type:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default = "i32")]
//!     type T;
//! }
//! let _: <MockTrait as Trait>::T = 42i32;
//! ```
//!
//! ## Attribute `#[moq(default = ...)]` const
//! Initializes or overrides default const value with specified value:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default = 42)]
//!     const CONST: i32;
//! }
//! assert_eq!(<MockTrait as Trait>::CONST, 42);
//! ```
//!
//! ## Attribute `#[moq(default_with = "...")]` const
//! Initializes or overrides default const value with specified const function:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default_with = "some_func")]
//!     const CONST: i32;
//! }
//! const fn some_func() -> i32 { 42 }
//! assert_eq!(MockTrait::CONST, 42);
//! ```
//!
//! ## Macro `moq::lambda!(...)`
//! Helpful macro for generating closure with specified lifetimes.
//! In rust you can't specify lifetimes for closure but you should, because lifetime
//! inference in closure is dumb. Then this macro will help.
//! Currently, capturing external variables is not possible, but this is in the plans.
//!
//! Syntax is like anonymous function:
//! ```
//! # fn main() {
//! #[derive(Debug, PartialEq)]
//! struct StructNamed {
//!    field: i32,
//! }
//!
//! impl StructNamed {
//! fn get_field(&self) -> i32 {
//!        self.field
//!    }
//! }
//!
//! struct StructUnnamed(i32);
//!
//! let arg1 = 1i32;
//! let arg2 = 2i32;
//! let arg3 = StructNamed { field: 3i32 };
//! let arg4 = StructNamed { field: 4i32 };
//! let arg5 = StructNamed { field: 5i32 };
//! let arg6 = StructNamed { field: 6i32 };
//! let arg7 = StructUnnamed(7i32);
//!
//! moq::lambda!(
//! // Environment capturing (optional)
//! //
//! //      expression          var alias expression type
//! //      |                   |         |
//! //      vvvvvvvvvvvvvvvv    vvvvvv    vvvvvvvvvvv
//!     [
//!         arg1                       => i32,
//!         arg2             as field0 => i32,
//!         arg3                       => StructNamed,
//!         arg4.field                 => i32,
//!         arg5.field       as field2 => i32,
//!         arg6.get_field() as field3 => i32,
//!         arg7.0           as field4 => i32
//!     ]
//!
//! // Function signature
//! //  
//! //  asyncness
//! //  |        generic params (lifetimes)
//! //  |        |    object that captured env, needs only if env specified
//! //  |        |    |      arguments
//! //  |        |    |      |                return type
//! //  |        |    |      |                |
//! //  vvvvv    vvvv vvvvv  vvvvvvvvvvvv     vvvvvvvvvvvv
//!     async fn <'a>(&self, arg: &'a str) -> &'static str {
//!         assert_eq!(self.arg1, 1);
//!         assert_eq!(self.field0, 2);
//!         assert_eq!(self.arg3, StructNamed { field: 3 });
//!         assert_eq!(self.field, 4);
//!         assert_eq!(self.field2, 5);
//!         assert_eq!(self.field3, 6);
//!         assert_eq!(self.field4, 7);
//!         // ...
//!         # unimplemented!()
//!     }
//! )
//! # ;
//! # }
//! ```
//!
//! It turns to something like that:
//! ```
//! # fn main() {
//! # struct StructNamed {
//! #    field: i32,
//! # }
//! # impl StructNamed {
//! # fn get_field(&self) -> i32 {
//! #        self.field
//! #    }
//! # }
//! # struct StructUnnamed(i32);
//! # let arg1 = 1i32;
//! # let arg2 = 2i32;
//! # let arg3 = StructNamed { field: 3i32 };
//! # let arg4 = StructNamed { field: 4i32 };
//! # let arg5 = StructNamed { field: 5i32 };
//! # let arg6 = StructNamed { field: 6i32 };
//! # let arg7 = StructUnnamed(7i32);
//! {
//!     struct __MoqLambda {
//!         arg1: i32,
//!         field0: i32,
//!         arg3: StructNamed,
//!         field: i32,
//!         field2: i32,
//!         field3: i32,
//!         field4: i32,
//!     }
//!     impl __MoqLambda {
//!         fn new(
//!             arg1: i32,
//!             field0: i32,
//!             arg3: StructNamed,
//!             field: i32,
//!             field2: i32,
//!             field3: i32,
//!             field4: i32,
//!         ) -> Self {
//!             Self {
//!                 arg1,
//!                 field0,
//!                 arg3,
//!                 field,
//!                 field2,
//!                 field3,
//!                 field4,
//!             }
//!         }
//!     }
//!     #[::moq::async_trait]
//!     impl<'a> ::moq::AsyncFunc<(&'a str,), &'static str> for __MoqLambda {
//!         async fn call<'__moq>(&'__moq self, (arg,): (&'a str,)) -> &'static str
//!         where (&'a str,): '__moq,
//!               &'static str: '__moq
//!         {
//!             // ...
//!             # unimplemented!()
//!         }
//!     }
//!     __MoqLambda::new(
//!         arg1,             
//!         arg2,             
//!         arg3,             
//!         arg4.field,       
//!         arg5.field,       
//!         arg6.get_field(),
//!         arg7.0,           
//!     )
//! }
//! # ;
//! # }
//! ```

pub use async_trait::async_trait;
pub use moq_derive::automock;
pub use moq_lambda::lambda;

pub trait Func<Args, Ret> {
    fn call<'a>(&'a self, args: Args) -> Ret
    where
        Args: 'a,
        Ret: 'a;
}

macro_rules! impl_func (
    ($($param:ident)*) => {
        impl<Function, $($param,)* Ret> Func<($($param,)*), Ret> for Function
        where
            Function: Fn($($param),*) -> Ret + Sync + 'static,
            $($param: Send,)*
        {
            #[inline]
            #[allow(non_snake_case)]
            fn call<'a>(&'a self, ($($param,)*): ($($param,)*)) -> Ret
            where
                $($param: 'a,)*
                Ret: 'a,
            {
                (self)($($param,)*)
            }
        }
    }
);

impl_func!();
impl_func!(A);
impl_func!(A B);
impl_func!(A B C);
impl_func!(A B C D);
impl_func!(A B C D E);
impl_func!(A B C D E F);
impl_func!(A B C D E F G);
impl_func!(A B C D E F G H);
impl_func!(A B C D E F G H I);
impl_func!(A B C D E F G H I J);
impl_func!(A B C D E F G H I J K);
impl_func!(A B C D E F G H I J K L);

#[async_trait::async_trait]
pub trait AsyncFunc<Args, Ret> {
    async fn call<'a>(&'a self, args: Args) -> Ret
    where
        Args: 'a,
        Ret: 'a;
}

macro_rules! impl_async_func (
    ($($param:ident)*) => {
        #[async_trait::async_trait]
        impl<Function, $($param,)* Ret, Fut> AsyncFunc<($($param,)*), Ret> for Function
        where
            Function: Fn($($param),*) -> Fut + Sync + 'static,
            Fut: std::future::Future<Output = Ret> + Send + 'static,
            $($param: Send,)*
        {
            #[inline]
            #[allow(non_snake_case)]
            async fn call<'a>(&'a self, ($($param,)*): ($($param,)*)) -> Ret
            where
                $($param: 'a,)*
                Ret: 'a,
            {
                (self)($($param,)*).await
            }
        }
    }
);

impl_async_func!();
impl_async_func!(A);
impl_async_func!(A B);
impl_async_func!(A B C);
impl_async_func!(A B C D);
impl_async_func!(A B C D E);
impl_async_func!(A B C D E F);
impl_async_func!(A B C D E F G);
impl_async_func!(A B C D E F G H);
impl_async_func!(A B C D E F G H I);
impl_async_func!(A B C D E F G H I J);
impl_async_func!(A B C D E F G H I J K);
impl_async_func!(A B C D E F G H I J K L);
