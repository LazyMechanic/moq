const fn def() -> i32 {
    2
}

#[moq::automock]
trait Trait {
    #[moq(default = 1)]
    #[moq(default_with = "def")]
    const CONST: i32;
}

fn main() {}