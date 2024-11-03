#[test]
fn test1() {
    #[moq::automock]
    trait Trait {
        fn f<A>(&self)
        where
            A: 'static;
    }

    let m = MockTrait::new().expect_f::<i32, _>(|| {});
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

    let m = MockTrait::new().expect_f(|arg: i32| assert_eq!(arg, 1));
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

    let m = MockTrait::new().expect_f(|| 1i32);
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

    let m = MockTrait::new().expect_f(|arg: i32| {
        assert_eq!(arg, 1);
        "2"
    });
    assert_eq!(m.f::<_, &'static str>(1i32), "2");
}

#[test]
fn test5() {
    struct Dummy<'a, T> {
        a: &'a str,
        b: T,
    }

    #[moq::automock]
    trait Trait {
        fn f<'a, A>(&self, arg: Dummy<'a, A>)
        where
            A: 'static;
    }

    let m = MockTrait::new().expect_f(|arg: Dummy<i32>| {
        assert_eq!(arg.a, "1");
        assert_eq!(arg.b, 2);
    });
    let a = String::from("1");
    m.f::<i32>(Dummy { a: &a, b: 2 });
}

#[test]
fn test6() {
    #[derive(Debug, Eq, PartialEq)]
    struct Dummy<'a, T> {
        a: &'a str,
        b: T,
    }

    #[moq::automock]
    trait Trait {
        fn f<'a, A>(&self, arg: Dummy<'a, A>) -> Dummy<'a, A>
        where
            A: 'static;
    }

    let m = MockTrait::new().expect_f(moq::lambda!(
        fn <'a>(arg: Dummy<'a, i32>) -> Dummy<'a, i32> {
            assert_eq!(arg.a, "1");
            assert_eq!(arg.b, 2);
            Dummy { a: arg.a, b: 3 }
        }
    ));
    let a = String::from("1");
    assert_eq!(m.f::<i32>(Dummy { a: &a, b: 2 }), Dummy { a: "1", b: 3 });
}
