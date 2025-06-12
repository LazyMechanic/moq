use std::future;

use async_stream::stream;
use futures::{FutureExt, StreamExt};

#[tokio::test]
async fn test1() {
    use future::Future;

    #[moq::automock]
    trait Trait {
        fn f(&self) -> impl Future<Output = i32>;
    }

    let m = MockTrait::new().expect_f(|| future::ready(42).boxed_local());
    assert_eq!(m.f().await, 42);
}

#[tokio::test]
async fn test2() {
    #[moq::automock]
    trait Trait {
        fn f(&self) -> impl future::Future<Output = i32>;
    }

    let m = MockTrait::new().expect_f(|| future::ready(42).boxed_local());
    assert_eq!(m.f().await, 42);
}

#[tokio::test]
async fn test3() {
    use future::Future;

    #[moq::automock]
    trait Trait {
        #[moq(output = "std::pin::Pin<_>")]
        fn f(&self) -> impl Future<Output = i32>;
    }

    let m = MockTrait::new().expect_f(|| future::ready(42).boxed_local());
    assert_eq!(m.f().await, 42);
}

#[tokio::test]
async fn test4() {
    use futures::Stream;

    #[moq::automock]
    trait Trait {
        fn f(&self) -> impl Stream<Item = i32>;
    }

    let m = MockTrait::new().expect_f(|| stream! { yield 42 }.boxed_local());
    assert_eq!(m.f().next().await, Some(42));
}

#[tokio::test]
async fn test5() {
    #[moq::automock]
    trait Trait {
        fn f(&self) -> impl futures::Stream<Item = i32>;
    }

    let m = MockTrait::new().expect_f(|| stream! { yield 42 }.boxed_local());
    assert_eq!(m.f().next().await, Some(42));
}
