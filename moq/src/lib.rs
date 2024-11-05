//! # Moq
//!
//! This library provides the procedural macro that generates a mock struct that implements trait.
//!
//! ## User guide
//! - [Getting started](#getting-started)
//! - [Mock object name](#mock-object-name)
//! - [`async_trait`](#async_trait)
//! - [Constants](#constants)
//! - [Associated types](#associated-types)
//! - [Default function implementation](#default-function-implementation)
//! - [Impl trait](#impl-trait)
//! - [Generics](#generics)
//! - [HRTB](#hrtb)
//! - [Restrictions](#restrictions)
//!
//! ### Getting started
//!
//! To get started, add an attribute macro to the trait for which you want to generate a mock object:
//! ```
//! #[moq::automock] // <-- this macro
//! trait TraitForMocking {
//!     fn func_that_i_want_to_test(&self, arg: String) -> Result<(), std::io::Error>;
//! }
//! ```
//!
//! Next, you can create a mock object and specify the expected behaviour:
//! ```
//! # #[moq::automock]
//! # trait TraitForMocking {
//! #    fn func_that_i_want_to_test(&self, arg: String) -> Result<(), std::io::Error>;
//! # }
//! #[test]
//! fn some_test() {
//!     let mock = MockTraitForMocking::new()
//!         .expect_func_that_i_want_to_test(|arg: String| {
//!             assert_eq!(arg, "Hello, World!");
//!             Ok(())
//!         });
//!
//!     assert!(mock.func_that_i_want_to_test("Hello, World!".to_owned()).is_ok());
//! }
//! ```
//!
//! #### How its work
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
//! ### Mock object name
//!
//! By default, mock object name will be `Mock%TRAIT%`, for example:
//! ```
//! // For trait:
//! #[moq::automock]
//! trait Trait { /*...*/ }
//!
//! // A mock will be generated:
//! # let _ = stringify!(
//! struct MockTrait { /*...*/ }
//! # );
//! # let _ = MockTrait::new();
//! ```
//!
//! If you want to customise it, then use `rename` attribute:
//! ```
//! #[moq::automock(rename = "FooBar")]
//! trait Trait { /*...*/ }
//!
//! // A mock will be generated:
//! # let _ = stringify!(
//! struct FooBar { /*...*/ }
//! # );
//! # let _ = FooBar::new();
//! ```
//!
//! You can also add an attribute directly to a trait:
//! ```
//! #[moq::automock]
//! #[moq(rename = "FooBar")]
//! trait Trait { /*...*/ }
//!
//! // A mock will be generated:
//! # let _ = stringify!(
//! struct FooBar { /*...*/ }
//! # );
//! # let _ = FooBar::new();
//! ```
//!
//! ### async_trait
//!
//! `automock` supports [`async_trait`](https://docs.rs/async-trait/latest/async_trait/):
//!
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
//!     let mock = MockTraitForMocking::new()
//!         .expect_func_that_i_want_to_test(|arg: String| async {
//!             assert_eq!(arg, "Hello, World!");
//!             Ok(())
//!         });
//!
//!     assert!(mock.func_that_i_want_to_test("Hello, World!".to_owned()).await.is_ok());
//! }
//! ```
//!
//! ### Constants
//!
//! There are 3 ways to specify the value of constants:
//! - By using the `default` attribute and specifying the 'as is' expression
//! - By using the `default_with` attribute and specifying the path to the constant function
//! - Through the default value
//!
//! ```
//! const fn const_str_default() -> &'static str { "string" }
//!
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default_with = "const_str_default")]
//!     const CONST_STR: &'static str;
//!     #[moq(default = 1)]
//!     const CONST_INT: i32;
//!     const CONST_DEFAULT: usize = 42;
//! }
//!
//! assert_eq!(MockTrait::CONST_STR, "string");
//! assert_eq!(MockTrait::CONST_INT, 1);
//! assert_eq!(MockTrait::CONST_DEFAULT, 42);
//! ```
//!
//! ### Associated types
//!
//! You can specify the type of associated type by using the `default` attribute and specifying the type:
//!
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default = "String")]
//!     type AssocType: Clone;
//! }
//!
//! let _: String = <MockTrait as Trait>::AssocType::from("string");
//! ```
//!
//! ### Default function implementation
//!
//! If your trait has default implementation of function, then by default `automock` generate independent implementation:
//!
//! ```
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!
//!     fn f2(&self) {
//!         self.f1();
//!     }
//! }
//!
//! let mock = MockTrait::new()
//!     .expect_f1(|| {})
//!     .expect_f2(|| {});
//!
//! mock.f1();
//! mock.f2();
//! // Success
//! ```
//!
//! If you want to use default implementation of function, you need to mark function by `default` attribute:
//!
//! ```
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!
//!     #[moq(default)]
//!     fn f2(&self) {
//!         self.f1();
//!     }
//! }
//!
//! let mock = MockTrait::new()
//!     .expect_f1(|| {})
//!     .expect_f1(|| {}); // <- attention here
//!
//! mock.f1();
//! mock.f2();
//! // Success
//! ```
//!
//! ### Impl trait
//!
//! If you using RPITIT (Return Position Impl Trait In Trait), `automock` generates mock with boxed type:
//!
//! ```rust
//! trait DummyTrait { fn dummy(&self) -> String; }
//! impl DummyTrait for i32 { fn dummy(&self) -> String { format!("{self}") } }
//! impl DummyTrait for &'static str { fn dummy(&self) -> String { format!("{self}") } }
//! impl DummyTrait for Box<dyn DummyTrait> { fn dummy(&self) -> String { (**self).dummy() } }
//!
//! #[moq::automock]
//! trait Trait {
//!     fn f(&self) -> impl DummyTrait;
//! }
//!
//! let mock = MockTrait::new()
//!     .expect_f(|| -> Box<dyn DummyTrait> {
//!         Box::new(42i32)
//!     })
//!     .expect_f(|| -> Box<dyn DummyTrait> {
//!         Box::new("string")
//!     });
//!
//! assert_eq!(mock.f().dummy(), "42");
//! assert_eq!(mock.f().dummy(), "string");
//! ```
//!
//! If boxed dyn trait doesn't suit you, you can specify a specific type by `output` attribute:
//!
//! ```
//! trait DummyTrait { fn dummy(&self) -> String; }
//! impl DummyTrait for i32 { fn dummy(&self) -> String { format!("{self}") } }
//!
//! #[moq::automock]
//! trait Trait {
//!     #[moq(output = "i32")]
//!     fn f(&self) -> impl DummyTrait;
//! }
//!
//! let mock = MockTrait::new()
//!     .expect_f(|| -> i32 { 42i32 });
//!
//! assert_eq!(mock.f().dummy(), "42");
//! ```
//!
//! `output` attribute also supports infer syntax, may be useful for async:
//!
//! ```
//! # use std::future::Future;
//! # use std::pin::Pin;
//!
//! #[moq::automock]
//! trait Trait {
//!     #[moq(output = "Pin<_>")]
//!     fn f(&self) -> impl Future<Output = i32>;
//! }
//!
//! # async fn t() {
//! let mock = MockTrait::new()
//!     .expect_f(|| -> Pin<Box<dyn Future<Output = i32>>> {
//!         Box::pin(async { 42i32 })
//!     });
//!
//! assert_eq!(mock.f().await, 42);
//! # }
//! ```
//!
//! ### Generics
//!
//! Mocking generic methods is possible, but there are restrictions:
//! - Generic parameter must be `'static` (maybe it will be fixed in the future)
//! - Expectation function generic must be specified explicitly
//!
//! ```
//! # use std::fmt::Display;
//!
//! #[moq::automock]
//! trait Trait {
//!     fn f<T>(&self, arg: T) where T: Display + 'static;
//! }
//!
//! let mock = MockTrait::new()
//!     .expect_f(|arg: i32| assert_eq!(arg, 123)); // <- explicit arg type
//!
//! mock.f(123);
//! ```
//!
//! ### HRTB
//!
//! In rust you can't specify lifetimes for closure but you should, because lifetime
//! inference in closure is dumb. Then [`moq::lambda!(...)`](crate::lambda) macro will help.
//!
//! ```
//! #[derive(Debug, Eq, PartialEq)]
//! struct Struct<'a>(&'a str);
//!
//! #[moq::automock]
//! #[async_trait::async_trait]
//! trait Trait {
//!    async fn f<'a, 'b>(
//!         &self,
//!         arg1: Struct<'a>,
//!         arg2: Struct<'b>,
//!    ) -> Struct<'b>;
//! }
//!
//! # async fn some_test() {
//! let mock = MockTrait::new()
//!     .expect_f(moq::lambda!(
//!         async fn <'a, 'b>(arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b> {
//!             assert_eq!(arg1, Struct("Hello"));
//!             assert_eq!(arg2, Struct("World"));
//!             arg2
//!         }
//!     ));
//!
//! assert_eq!(
//!     mock.f(
//!         Struct("Hello"),
//!         Struct("World"),
//!     ).await,
//!     Struct("World"),
//! );
//! # }
//! ```
//!
//! ### Restrictions
//!
//! - Generics must be `T: 'static`
//! - Expectation function with generics should accept specific types (`mock.expect_f::<T, _>(|arg: T| {})`)
//! - Anonymized lifetimes not allowed (`&'_ T`), make them explicit
//! - Impl trait in argument position not allowed (`arg: impl Trait`), use generics instead
//! - Static functions not supported yet

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
