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
//! #[moq::automock] // <- the main limitation is to specify the `automock` macro above the `async_trait`
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
//! ## How its work
//! Generated mock struct checks that all specified expectation will calls in specified order.
//! If `f1()` is called while `f2()` is expected, then there will be a panic.
//! ```
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!     fn f2(&self);
//! }
//! let mock = TraitMock::new().expect_f2(||{});
//! mock.f1() // Panic
//! ```
//!
//! Checks if all expected functions are being called, if not then will be a panic.
//! ```
//! #[moq::automock]
//! trait Trait {
//!     fn f1(&self);
//!     fn f2(&self);
//! }
//! let mock = TraitMock::new()
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
//! let mock = TraitMock::new().expect_f1(|| {});
//! mock.def(); // Success
//! ```
//!
//! ## Attribute `#[moq(default = "...")]` associative type
//! Sets the associative type:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default = "i32"]
//!     type T;
//! }
//! let _: TraitMock::T = 42i32;
//! ```
//!
//! ## Attribute `#[moq(default = ...)]` const
//! Initializes or overrides default const value with specified value:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default = 42]
//!     const CONST: i32;
//! }
//! assert_eq!(TraitMock::CONST, 42);
//! ```
//!
//! ## Attribute `#[moq(default_with = "...")]` const
//! Initializes or overrides default const value with specified const function:
//! ```
//! #[moq::automock]
//! trait Trait {
//!     #[moq(default_with = "some_func"]
//!     const CONST: i32;
//! }
//! const fn some_func() -> i32 { 42 }
//! assert_eq!(TraitMock::CONST, 42);
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
