#[test]
fn test1() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn () {}));
}

#[test]
fn test2() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(i32,), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn (arg: i32) {
        let _arg = arg;
    }));
}

#[test]
fn test3() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(), i32> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn () -> i32 {
        0
    }));
}

#[test]
fn test4() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(i32,), i32> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn (arg: i32) -> i32 {
        let _arg = arg;
        0
    }));
}

#[test]
fn test5() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(i32, String), (i32, String)> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn (arg1: i32, arg2: String) -> (i32, String) {
        (arg1, arg2)
    }));
}

#[test]
fn test6() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a> moq::Func<(Struct<'a>,), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn <'a>(arg: Struct<'a>) {
        let _arg = arg;
    }));
}

#[test]
fn test7() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: moq::Func<(), Struct<'static>> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn () -> Struct<'static> {
        Struct("1")
    }));
}

#[test]
fn test8() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a> moq::Func<(Struct<'a>,), Struct<'a>> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(fn <'a>(arg: Struct<'a>) -> Struct<'a> {
        arg
    }));
}

#[test]
fn test9() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a, 'b> moq::Func<(Struct<'a>, Struct<'b>), Struct<'b>> + Send + Sync + 'static,
    {
    }
    assert_lambda(
        moq::lambda!(fn <'a, 'b>(arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b> {
            let _arg1 = arg1;
            arg2
        }),
    );
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[test]
fn test10() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn () {}));
}

#[test]
fn test11() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(i32,), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn (arg: i32) {
        let _arg = arg;
    }));
}

#[test]
fn test12() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(), i32> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn () -> i32 {
        0
    }));
}

#[test]
fn test13() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(i32,), i32> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn (arg: i32) -> i32 {
        let _arg = arg;
        0
    }));
}

#[test]
fn test14() {
    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(i32, String), (i32, String)> + Send + Sync + 'static,
    {
    }
    assert_lambda(
        moq::lambda!(async fn (arg1: i32, arg2: String) -> (i32, String) {
            (arg1, arg2)
        }),
    );
}

#[test]
fn test15() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a> moq::AsyncFunc<(Struct<'a>,), ()> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn <'a>(arg: Struct<'a>) {
        let _arg = arg;
    }));
}

#[test]
fn test16() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: moq::AsyncFunc<(), Struct<'static>> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn () -> Struct<'static> {
        Struct("1")
    }));
}

#[test]
fn test17() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a> moq::AsyncFunc<(Struct<'a>,), Struct<'a>> + Send + Sync + 'static,
    {
    }
    assert_lambda(moq::lambda!(async fn <'a>(arg: Struct<'a>) -> Struct<'a> {
        arg
    }));
}

#[test]
fn test18() {
    #[derive(Debug, Eq, PartialEq)]
    struct Struct<'a>(&'a str);

    fn assert_lambda<F>(_: F)
    where
        F: for<'a, 'b> moq::AsyncFunc<(Struct<'a>, Struct<'b>), Struct<'b>> + Send + Sync + 'static,
    {
    }
    assert_lambda(
        moq::lambda!(async fn <'a, 'b>(arg1: Struct<'a>, arg2: Struct<'b>) -> Struct<'b> {
            let _arg1 = arg1;
            arg2
        }),
    );
}
