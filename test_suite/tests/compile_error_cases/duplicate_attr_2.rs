#[moq::automock]
trait Trait {
    #[moq(default = "i32")]
    #[moq(default = "i64")]
    type Type;
}

fn main() {}
