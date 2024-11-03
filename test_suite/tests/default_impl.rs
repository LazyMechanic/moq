#[test]
fn test1() {
    #[moq::automock]
    trait Trait {
        fn f1(&self);
        #[moq(default)]
        fn f2(&self) {
            self.f1();
        }
    }

    let m = MockTrait::new().expect_f1(|| {}).expect_f1(|| {});
    m.f1();
    m.f2();
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait {
        fn f1(&self);
        fn f2(&self) {
            self.f1();
        }
    }

    let m = MockTrait::new().expect_f1(|| {}).expect_f2(|| {});
    m.f1();
    m.f2();
}

#[tokio::test]
async fn test3() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f1(&self);
        #[moq(default)]
        async fn f2(&self) {
            self.f1().await;
        }
    }

    let m = MockTrait::new()
        .expect_f1(|| async {})
        .expect_f1(|| async {});
    m.f1().await;
    m.f2().await;
}

#[tokio::test]
async fn test4() {
    #[moq::automock]
    #[async_trait::async_trait]
    trait Trait {
        async fn f1(&self);
        async fn f2(&self) {
            self.f1().await;
        }
    }

    let m = MockTrait::new()
        .expect_f1(|| async {})
        .expect_f2(|| async {});
    m.f1().await;
    m.f2().await;
}
