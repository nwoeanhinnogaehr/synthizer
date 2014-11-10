use super::function::Function;
use super::CompileError;
use super::scope::Scope;

struct Sum<'a> {
	fns: Vec<Box<Function + 'a>>,
}

impl<'a> Sum<'a> {
	fn eval(&self, scope: &'a Scope) -> Result<f32, CompileError> {
		let mut sum = 0_f32;
		for f in self.fns.iter() {
			sum += try!(f.call(scope));
		}
		Ok(sum)
	}
}

