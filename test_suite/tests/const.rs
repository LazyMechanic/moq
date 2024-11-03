#[test]
fn test1() {
    #[moq::automock]
    trait Trait {
        const CONST: i32 = 1;
    }

    assert_eq!(MockTrait::CONST, 1);
}

#[test]
fn test2() {
    #[moq::automock]
    trait Trait {
        #[moq(default = 1i32)]
        const CONST: i32;
    }

    assert_eq!(MockTrait::CONST, 1);
}

#[test]
fn test3() {
    #[moq::automock]
    trait Trait {
        #[moq(default = 1i32)]
        const CONST: i32 = 2;
    }

    assert_eq!(MockTrait::CONST, 1);
}

#[test]
fn test4() {
    #[moq::automock]
    trait Trait {
        #[moq(default_with = "make_const")]
        const CONST: i32;
    }

    const fn make_const() -> i32 {
        1
    }

    assert_eq!(MockTrait::CONST, 1);
}

#[test]
fn test5() {
    #[moq::automock]
    trait Trait {
        #[moq(default_with = "make_const")]
        const CONST: i32 = 2;
    }

    const fn make_const() -> i32 {
        1
    }

    assert_eq!(MockTrait::CONST, 1);
}

#[test]
fn test6() {
    #[moq::automock]
    trait Trait {
        const CONST1: i32 = 1;
        const CONST2: i32 = Self::CONST1;
    }

    assert_eq!(MockTrait::CONST2, MockTrait::CONST1);
}

#[test]
fn test7() {
    #[moq::automock]
    trait Trait {
        const CONST1: i32 = 1i32;
        #[moq(default = Self::CONST1)]
        const CONST2: i32;
    }

    assert_eq!(MockTrait::CONST2, MockTrait::CONST1);
}
