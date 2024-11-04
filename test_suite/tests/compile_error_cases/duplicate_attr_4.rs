#[moq::automock]
trait Trait {
    #[moq(default = 1)]
    #[moq(default = 2)]
    const CONST: i32;
}

fn main() {}
