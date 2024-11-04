const fn def() -> i32 {
    2
}

#[moq::automock]
trait Trait {
    #[moq(default_with = "def")]
    #[moq(default = 1)]
    const CONST: i32;
}

fn main() {}
