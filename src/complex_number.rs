use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, Copy)]
pub struct ComplexNumber<T> {
    pub real: T,
    pub imag: T,
}

macro_rules! ComplexNumber {
    ($T:ty) => {
        impl ComplexNumber<$T> {
            pub fn one() -> Self {
                Self { real: 1.0, imag: 0.0 }
            }

            pub fn from_polar(norm: $T, angle: $T) -> Self {
                Self {
                    real: norm * angle.cos(),
                    imag: norm * angle.sin(),
                }
            }

            pub fn from_sqrt_real(real: $T) -> Self {
                if real < 0.0 {
                    Self {
                        real: 0.0,
                        imag: (-real).sqrt(),
                    }
                } else {
                    Self {
                        real: real.sqrt(),
                        imag: 0.0,
                    }
                }
            }

            pub fn conjugate(&self) -> Self {
                Self {
                    real: self.real,
                    imag: -self.imag,
                }
            }

            pub fn squared_abs(&self) -> $T {
                self.real * self.real + self.imag * self.imag
            }

            pub fn abs(&self) -> $T {
                self.squared_abs().sqrt()
            }

            pub fn arg(&self) -> $T {
                self.imag.atan2(self.real)
            }

            /*pub fn sign(&self) -> Self {
                *self * (1.0 / self.abs())
            }*/

            /*pub fn pow(&self, rhs: &Self) -> Self {
                let (abs, arg) = (self.abs(), self.arg());
                return Self::from_polar(
                    abs.powf(rhs.real)*(-arg*rhs.imag).exp(),
                    arg*rhs.real+abs.ln()*rhs.imag
                );
            }*/

            pub fn powf(&self, rhs: $T) -> Self {
                return Self::from_polar(self.abs().powf(rhs), self.arg() * rhs);
            }

            pub fn powi(&self, rhs: isize) -> Self {
                // https://en.wikipedia.org/wiki/Exponentiation_by_squaring
                if rhs == 0 {
                    return Self::one();
                }
                let mut n = rhs;
                let mut x = if n < 0 {
                    n = -n;
                    Self::one() / *self
                } else {
                    *self
                };
                let mut y = Self::one();
                while n > 1 {
                    if n & 1 == 1 {
                        y = x * y;
                    }
                    x = x * x;
                    n >>= 1;
                }
                x * y
            }
        }

        impl From<$T> for ComplexNumber<$T> {
            fn from(real: $T) -> Self {
                Self { real, imag: 0.0 }
            }
        }

        impl Add<$T> for ComplexNumber<$T> {
            type Output = Self;

            fn add(self, rhs: $T) -> Self::Output {
                Self::Output {
                    real: self.real + rhs,
                    imag: self.imag,
                }
            }
        }

        impl Sub<$T> for ComplexNumber<$T> {
            type Output = Self;

            fn sub(self, rhs: $T) -> Self::Output {
                Self::Output {
                    real: self.real - rhs,
                    imag: self.imag,
                }
            }
        }

        impl Add for ComplexNumber<$T> {
            type Output = Self;

            fn add(self, rhs: Self) -> Self::Output {
                Self::Output {
                    real: self.real + rhs.real,
                    imag: self.imag + rhs.imag,
                }
            }
        }

        impl Sub for ComplexNumber<$T> {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self::Output {
                Self::Output {
                    real: self.real - rhs.real,
                    imag: self.imag - rhs.imag,
                }
            }
        }

        impl Neg for ComplexNumber<$T> {
            type Output = Self;

            fn neg(self) -> Self::Output {
                Self::Output {
                    real: -self.real,
                    imag: -self.imag,
                }
            }
        }

        impl Mul<$T> for ComplexNumber<$T> {
            type Output = Self;

            fn mul(self, rhs: $T) -> Self::Output {
                Self::Output {
                    real: self.real * rhs,
                    imag: self.imag * rhs,
                }
            }
        }

        impl Div<$T> for ComplexNumber<$T> {
            type Output = Self;

            fn div(self, rhs: $T) -> Self::Output {
                self * (1.0 / rhs)
            }
        }

        impl Mul for ComplexNumber<$T> {
            type Output = Self;

            fn mul(self, rhs: Self) -> Self::Output {
                Self::Output {
                    real: self.real * rhs.real - self.imag * rhs.imag,
                    imag: self.imag * rhs.real + self.real * rhs.imag,
                }
            }
        }

        impl Div for ComplexNumber<$T> {
            type Output = Self;

            fn div(self, rhs: Self) -> Self::Output {
                self * rhs.conjugate() / rhs.squared_abs()
            }
        }
    };
}

ComplexNumber!(f32);

impl From<glam::Vec2> for ComplexNumber<f32> {
    fn from(vec: glam::Vec2) -> Self {
        Self { real: vec[0], imag: vec[1] }
    }
}

impl Into<glam::Vec2> for ComplexNumber<f32> {
    fn into(self) -> glam::Vec2 {
        glam::vec2(self.real, self.imag)
    }
}
