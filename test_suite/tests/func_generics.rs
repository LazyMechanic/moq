#[test]
fn test1() {
    #[moq::automock]
    trait Trait {
        fn f<A>(&self)
        where
            A: 'static;
    }

    let m = TraitMock::new().expect_f::<i32, _>(|| {});
    m.f::<i32>();
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait {
        fn f<A>(&self, arg: A)
        where
            A: 'static;
    }

    let m = TraitMock::new().expect_f(|arg: i32| assert_eq!(arg, 1));
    m.f(1i32);
}

#[test]
fn test3() {
    #[moq::automock]
    trait Trait {
        fn f<A>(&self) -> A
        where
            A: 'static;
    }

    let m = TraitMock::new().expect_f(|| 1i32);
    assert_eq!(m.f::<i32>(), 1i32);
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait {
        fn f<A, B>(&self, arg: A) -> B
        where
            A: 'static,
            B: 'static;
    }

    let m = TraitMock::new().expect_f(|arg: i32| {
        assert_eq!(arg, 1);
        "2"
    });
    assert_eq!(m.f::<_, &'static str>(1i32), "2");
}
