#[tokio::test]
async fn test1() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = MockTrait::new().expect_f(|| async {});
    m.f().await;
}

#[tokio::test]
async fn test2() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = MockTrait::new().expect_f(|| async {});

    tokio::spawn(async move {
        m.f().await;
    })
    .await
    .unwrap();
}

#[tokio::test]
async fn test3() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self, arg: i32);
    }

    let m = MockTrait::new().expect_f(|arg| async move {
        assert_eq!(arg, 1);
    });
    m.f(1).await;
}

#[tokio::test]
async fn test4() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self) -> i32;
    }

    let m = MockTrait::new().expect_f(|| async move { 1 });
    assert_eq!(m.f().await, 1);
}

#[tokio::test]
async fn test5() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self, arg: i32) -> i32;
    }

    let m = MockTrait::new().expect_f(|arg| async move {
        assert_eq!(arg, 1);
        arg
    });
    assert_eq!(m.f(1).await, 1);
}

#[tokio::test]
async fn test6() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self, arg1: i32, arg2: String) -> i32;
    }

    let m = MockTrait::new().expect_f(|arg1, arg2| async move {
        assert_eq!(arg1, 1);
        assert_eq!(arg2, "2");
        arg1
    });
    assert_eq!(m.f(1, "2".to_owned()).await, 1);
}

#[tokio::test]
async fn test7() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f<'a>(&self, arg: Struct<'a>);
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        async fn <'a>(arg: Struct<'a>) {
            assert_eq!(arg, Struct("1"));
        }
    ));

    m.f(Struct("1")).await;
}

#[tokio::test]
async fn test8() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self) -> Struct<'static>;
    }

    let m = MockTrait::new().expect_f(|| async move { Struct("1") });
    assert_eq!(m.f().await, Struct("1"));
}

#[tokio::test]
async fn test9() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f<'a>(&self, arg: Struct<'a>) -> Struct<'a>;
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        async fn <'a>(arg: Struct<'a>) -> Struct<'a> {
            assert_eq!(arg, Struct("1"));
            arg
        }
    ));
    assert_eq!(m.f(Struct("1")).await, Struct("1"));
}

#[tokio::test]
async fn test10() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f<'a, 'b>(&self, arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b>;
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        async fn <'a, 'b>(arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b> {
            assert_eq!(arg1, Struct("1"));
            assert_eq!(arg2, Struct("2"));
            arg2
        }
    ));
    assert_eq!(m.f(Struct("1"), Struct("2")).await, Struct("2"));
}
