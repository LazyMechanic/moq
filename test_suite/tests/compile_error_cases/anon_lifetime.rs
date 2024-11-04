#[moq::automock]
trait Trait {
    fn f(&self, arg: &'_ i32);
}

fn main() {}
