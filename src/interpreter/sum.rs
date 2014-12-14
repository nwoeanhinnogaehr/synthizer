use super::function::Function;
use super::CompileError;
use super::scope::CowScope;

/// An object representing the sum of a number of functions. Used internally as the additive part
/// of the additive synthesis process.
pub struct Sum<'a> {
	pub fns: Vec<Box<Function + 'a>>,
}

impl<'a> Sum<'a> {
	pub fn new(fns: Vec<Box<Function + 'a>>) -> Sum<'a> {
		Sum {
			fns: fns,
		}
	}

	// TODO parallelize
	/// Sums up all the functions
	pub fn eval<'s>(&self, scope: CowScope<'s>) -> Result<f32, CompileError> {
		let mut sum = 0_f32;
		for f in self.fns.iter() {
			sum += try!(f.call(scope.clone()));
		}
		Ok(sum)
	}
}

