#[moq::automock]
trait Trait {
    #[moq(default = "i32", default = "i64")]
    type Type;
}

fn main() {}
