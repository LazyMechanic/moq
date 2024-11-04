#[moq::automock]
trait Trait {
    #[moq(default = 1, default = 2)]
    const CONST: i32;
}

fn main() {}
