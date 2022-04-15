#[test]
#[should_panic]
fn test1() {
    #[moq::automock]
    trait Trait {
        fn f(&self);
    }

    let _m = TraitMock::new().expect_f(|| {});
}

#[test]
#[should_panic]
fn test2() {
    #[moq::automock]
    trait Trait {
        fn f(&self);
    }

    let m = TraitMock::new();
    m.f();
}

#[test]
#[should_panic]
fn test3() {
    #[moq::automock]
    trait Trait {
        fn f(&self);
    }

    let m = TraitMock::new().expect_f(|| {});
    m.f();
    m.f();
}

#[tokio::test]
#[should_panic]
async fn test4() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let _m = TraitMock::new().expect_f(|| async {});
}

#[allow(unused_must_use)]
#[tokio::test]
#[should_panic]
async fn test5() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = TraitMock::new().expect_f(|| async {});
    m.f();
}

#[tokio::test]
#[should_panic]
async fn test6() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = TraitMock::new().expect_f(|| async {}).expect_f(|| async {});
    m.f().await;
}
