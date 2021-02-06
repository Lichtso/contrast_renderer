use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, Copy)]
pub struct ComplexNumber<T> {
    pub real: T,
    pub imag: T,
}

macro_rules! ComplexNumber {
    ($T:ty) => {
        impl ComplexNumber<$T> {
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

            pub fn squared_norm(&self) -> $T {
                self.real * self.real + self.imag * self.imag
            }

            pub fn norm(&self) -> $T {
                self.squared_norm().sqrt()
            }

            pub fn angle(&self) -> $T {
                self.imag.atan2(self.real)
            }

            /*pub fn exp(&self, rhs: &Self) -> Self {
                let (norm, angle) = (self.norm(), self.angle());
                return Self::from_polar(
                    norm.powf(rhs.real)*(-angle*rhs.imag).exp(),
                    angle*rhs.real+norm.ln()*rhs.imag
                );
            }*/

            pub fn exp_real(&self, rhs: $T) -> Self {
                return Self::from_polar(self.norm().powf(rhs), self.angle() * rhs);
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
                self * rhs.conjugate() / rhs.squared_norm()
            }
        }
    };
}

ComplexNumber!(f32);
