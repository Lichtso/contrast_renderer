//! Floats which are guaranteed to be finite (no NaNs or Infinity)

use geometric_algebra::{ppga2d, ppga3d};

/// Unlike `f32` and `f64` this guarantees that `x == x` so it can be used to sort and hash
#[derive(Clone, Copy)]
pub struct SafeFloat<DataType, const N: usize> {
    values: [DataType; N],
}

macro_rules! implement {
    ($f_type:ty, 1) => {
        impl Default for SafeFloat<$f_type, 1> {
            fn default() -> Self {
                Self { values: [0.0] }
            }
        }

        impl std::fmt::Debug for SafeFloat<$f_type, 1> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.values.fmt(f)
            }
        }

        impl SafeFloat<$f_type, 1> {
            /// Returns the unpacked value
            pub fn unwrap(self) -> $f_type {
                self.values[0]
            }
        }

        impl std::convert::From<SafeFloat<$f_type, 1>> for $f_type {
            fn from(safe_float: SafeFloat<$f_type, 1>) -> Self {
                safe_float.values[0]
            }
        }

        impl std::convert::From<&SafeFloat<$f_type, 1>> for $f_type {
            fn from(safe_float: &SafeFloat<$f_type, 1>) -> Self {
                safe_float.values[0]
            }
        }

        impl std::convert::From<$f_type> for SafeFloat<$f_type, 1> {
            fn from(mut value: $f_type) -> Self {
                assert!(value.is_finite());
                if value.to_bits() == 1 << (std::mem::size_of::<$f_type>() * 8 - 1) {
                    value = 0.0;
                }
                Self { values: [value] }
            }
        }

        impl std::convert::From<&$f_type> for SafeFloat<$f_type, 1> {
            fn from(value: &$f_type) -> Self {
                Self::from(*value)
            }
        }

        impl std::hash::Hash for SafeFloat<$f_type, 1> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.values[0].to_bits().hash(state);
            }
        }

        impl PartialEq for SafeFloat<$f_type, 1> {
            fn eq(&self, other: &Self) -> bool {
                self.values[0].eq(&other.values[0])
            }
        }

        impl Eq for SafeFloat<$f_type, 1> {}

        impl PartialOrd for SafeFloat<$f_type, 1> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for SafeFloat<$f_type, 1> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.values[0].partial_cmp(&other.values[0]).unwrap()
            }
        }
    };
    ($f_type:ty, $n:literal) => {
        impl Default for SafeFloat<$f_type, $n> {
            fn default() -> Self {
                Self { values: [0.0; $n] }
            }
        }

        impl std::fmt::Debug for SafeFloat<$f_type, $n> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.values.fmt(f)
            }
        }

        impl std::convert::From<SafeFloat<$f_type, $n>> for [$f_type; $n] {
            fn from(safe_float: SafeFloat<$f_type, $n>) -> Self {
                safe_float.values
            }
        }

        impl std::convert::From<&SafeFloat<$f_type, $n>> for [$f_type; $n] {
            fn from(safe_float: &SafeFloat<$f_type, $n>) -> Self {
                safe_float.values
            }
        }

        impl std::convert::From<[$f_type; $n]> for SafeFloat<$f_type, $n> {
            fn from(mut values: [$f_type; $n]) -> Self {
                for value in values.iter_mut() {
                    assert!(value.is_finite());
                    if value.to_bits() == 1 << (std::mem::size_of::<$f_type>() * 8 - 1) {
                        *value = 0.0;
                    }
                }
                Self { values }
            }
        }

        impl SafeFloat<$f_type, $n> {
            /// Returns the unpacked values
            pub fn unwrap(self) -> [$f_type; $n] {
                self.values
            }
        }

        impl std::convert::From<&[$f_type; $n]> for SafeFloat<$f_type, $n> {
            fn from(values: &[$f_type; $n]) -> Self {
                Self::from(*values)
            }
        }

        impl std::hash::Hash for SafeFloat<$f_type, $n> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                for value in self.values.iter() {
                    value.to_bits().hash(state);
                }
            }
        }

        impl PartialEq for SafeFloat<$f_type, $n> {
            fn eq(&self, other: &Self) -> bool {
                for (a, b) in self.values.iter().zip(other.values.iter()) {
                    if !a.eq(b) {
                        return false;
                    }
                }
                true
            }
        }

        impl Eq for SafeFloat<$f_type, $n> {}

        impl PartialOrd for SafeFloat<$f_type, $n> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for SafeFloat<$f_type, $n> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                for (a, b) in self.values.iter().zip(other.values.iter()) {
                    let order = a.partial_cmp(b).unwrap();
                    if order != std::cmp::Ordering::Equal {
                        return order;
                    }
                }
                std::cmp::Ordering::Equal
            }
        }
    };
}

implement!(f32, 1);
implement!(f32, 2);
implement!(f32, 3);
implement!(f32, 4);

implement!(f64, 1);
implement!(f64, 2);
implement!(f64, 3);
implement!(f64, 4);

impl std::convert::From<SafeFloat<f32, 3>> for ppga2d::Point {
    fn from(safe_float: SafeFloat<f32, 3>) -> Self {
        Self::from(safe_float.unwrap())
    }
}

impl std::convert::From<ppga2d::Point> for SafeFloat<f32, 3> {
    fn from(point: ppga2d::Point) -> Self {
        [point[0], point[1], point[2]].into()
    }
}

impl std::convert::From<SafeFloat<f32, 4>> for ppga3d::Point {
    fn from(safe_float: SafeFloat<f32, 4>) -> Self {
        Self::from(safe_float.unwrap())
    }
}

impl std::convert::From<ppga3d::Point> for SafeFloat<f32, 4> {
    fn from(point: ppga3d::Point) -> Self {
        [point[0], point[1], point[2], point[3]].into()
    }
}

impl std::convert::From<SafeFloat<f32, 4>> for ppga2d::Motor {
    fn from(safe_float: SafeFloat<f32, 4>) -> Self {
        Self::from(safe_float.unwrap())
    }
}

impl std::convert::From<ppga2d::Motor> for SafeFloat<f32, 4> {
    fn from(motor: ppga2d::Motor) -> Self {
        [motor[0], motor[1], motor[2], motor[3]].into()
    }
}
