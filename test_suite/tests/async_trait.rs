#[tokio::test]
async fn test1() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = TraitMock::new().expect_f(|| async {});
    m.f().await;
}

#[tokio::test]
async fn test2() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f(&self);
    }

    let m = TraitMock::new().expect_f(|| async {});

    tokio::spawn(async move {
        m.f().await;
    })
    .await
    .unwrap();
}
