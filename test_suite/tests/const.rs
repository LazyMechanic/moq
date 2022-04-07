#[derive(Debug, Eq, PartialEq)]
pub struct Struct(i32);

impl Struct {
    pub const fn new(arg: i32) -> Self {
        Self(arg)
    }
}

mod some {
    pub mod path {
        use crate::Struct;

        pub const fn mock_struct() -> Struct {
            Struct::new(3)
        }
    }
}

#[moq::automock]
trait Trait {
    #[moq(value = 1)]
    const CONST_I32: i32;
    #[moq(value = "2")]
    const CONST_STR: &'static str;
    #[moq(value_with = "some::path::mock_struct")]
    const CONST_STRUCT: Struct;

    const CONST_I32_DEF: i32 = 1;
    const CONST_STR_DEF: &'static str = "2";
    const CONST_STRUCT_DEF: Struct = Struct::new(3);

    #[moq(value = 1)]
    const CONST_I32_DEF_OVERWRITE: i32 = 1;
    #[moq(value = "2")]
    const CONST_STR_DEF_OVERWRITE: &'static str = "2";
    #[moq(value_with = "some::path::mock_struct")]
    const CONST_STRUCT_DEF_OVERWRITE: Struct = Struct::new(3);
}

#[test]
fn test() {
    assert_eq!(TraitMock::CONST_I32, 1);
    assert_eq!(TraitMock::CONST_STR, "2");
    assert_eq!(TraitMock::CONST_STRUCT, Struct(3));

    assert_eq!(TraitMock::CONST_I32_DEF, 1);
    assert_eq!(TraitMock::CONST_STR_DEF, "2");
    assert_eq!(TraitMock::CONST_STRUCT_DEF, Struct(3));

    assert_eq!(TraitMock::CONST_I32_DEF_OVERWRITE, 1);
    assert_eq!(TraitMock::CONST_STR_DEF_OVERWRITE, "2");
    assert_eq!(TraitMock::CONST_STRUCT_DEF_OVERWRITE, Struct(3));
}
