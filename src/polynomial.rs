//! Solving polynomials up to degree 4

#![allow(clippy::many_single_char_names)]
use crate::error::ERROR_MARGIN;
use geometric_algebra::{epga1d::*, Reversal, SquaredMagnitude};

/// Represents a complex root as homogeneous coordinates
#[derive(Debug, Clone, Copy)]
pub struct Root {
    /// Real part of the complex numerator
    pub numerator_real: f32,
    /// Imaginary part of the complex numerator
    pub numerator_imag: f32,
    /// Real denominator
    pub denominator: f32,
}

impl Root {
    pub fn new(numerator_real: f32, numerator_imag: f32, denominator: f32) -> Self {
        Self {
            numerator_real,
            numerator_imag,
            denominator,
        }
    }
}

/// Finds the discriminant and root of a degree 1 polynomial.
///
/// `0 = coefficients[1] * x + coefficients[0]`
pub fn solve_linear(coefficients: [f32; 2]) -> (f32, Vec<Root>) {
    if coefficients[1].abs() <= ERROR_MARGIN {
        (0.0, vec![])
    } else {
        (1.0, vec![Root::new(-coefficients[0], 0.0, coefficients[1])])
    }
}

/// Finds the discriminant and roots of a degree 2 polynomial.
///
/// `0 = coefficients[2] * x.powi(2) + coefficients[1] * x + coefficients[0]`
pub fn solve_quadratic(coefficients: [f32; 3]) -> (f32, Vec<Root>) {
    if coefficients[2].abs() <= ERROR_MARGIN {
        return solve_linear([coefficients[0], coefficients[1]]);
    }
    // https://en.wikipedia.org/wiki/Quadratic_formula
    let discriminant = coefficients[1].powi(2) - 4.0 * coefficients[2] * coefficients[0];
    let q = Scalar::new(discriminant).sqrt();
    let mut solutions = Vec::with_capacity(3);
    for s in &[-q, q] {
        let numerator = *s - ComplexNumber::new(coefficients[1], 0.0);
        solutions.push(Root::new(numerator.real(), numerator.imaginary(), 2.0 * coefficients[2]));
    }
    (discriminant, solutions)
}

const ROOTS_OF_UNITY_3: [ComplexNumber; 3] = [
    // 0.8660254037844386467637231707529361834714026269051903140279034897
    // ComplexNumber::from_polar(1.0, -120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 0.0),
    ComplexNumber::new(-0.5, -0.8660254),
    ComplexNumber::new(-0.5, 0.8660254),
    ComplexNumber::new(1.0, 0.0),
];

/// Finds the discriminant and roots of a degree 3 polynomial.
///
/// `0 = coefficients[3] * x.powi(3) + coefficients[2] * x.powi(2) + coefficients[1] * x + coefficients[0]`
///
/// Also returns the index of the real root if there are two complex roots and one real root.
pub fn solve_cubic(coefficients: [f32; 4]) -> (f32, Vec<Root>, usize) {
    if coefficients[3].abs() <= ERROR_MARGIN {
        let (discriminant, roots) = solve_quadratic([coefficients[0], coefficients[1], coefficients[2]]);
        return (discriminant, roots, 2);
    }
    // https://en.wikipedia.org/wiki/Cubic_equation
    let d = [
        coefficients[2].powi(2) - 3.0 * coefficients[3] * coefficients[1],
        2.0 * coefficients[2].powi(3) - 9.0 * coefficients[3] * coefficients[2] * coefficients[1] + 27.0 * coefficients[3].powi(2) * coefficients[0],
    ];
    let mut solutions = Vec::with_capacity(3);
    let discriminant = d[1].powi(2) - 4.0 * d[0].powi(3);
    let c = Scalar::new(discriminant).sqrt();
    let c = ((c + ComplexNumber::new(if c.real() + d[1] == 0.0 { -d[1] } else { d[1] }, 0.0)) * Scalar::new(0.5)).powf(1.0 / 3.0);
    for root_of_unity in &ROOTS_OF_UNITY_3 {
        let ci = c * *root_of_unity;
        let denominator = ci * Scalar::new(3.0 * coefficients[3]);
        let numerator = (ci * Scalar::new(-coefficients[2]) - ci * ci - ComplexNumber::new(d[0], 0.0)) * denominator.reversal();
        solutions.push(Root::new(numerator.real(), numerator.imaginary(), denominator.squared_magnitude().real()));
    }
    let real_root = (((std::f32::consts::PI - c.arg()) / (std::f32::consts::PI * 2.0 / 3.0)) as usize + 1) % 3;
    (discriminant, solutions, real_root)
}

/// Finds the discriminant and roots of a degree 4 polynomial.
///
/// `0 = coefficients[4] * x.powi(4) + coefficients[3] * x.powi(3) + coefficients[2] * x.powi(2) + coefficients[1] * x + coefficients[0]`
pub fn solve_quartic(coefficients: [f32; 5]) -> (f32, Vec<Root>) {
    if coefficients[4].abs() <= ERROR_MARGIN {
        let (discriminant, roots, _real_root) = solve_cubic([coefficients[0], coefficients[1], coefficients[2], coefficients[3]]);
        return (discriminant, roots);
    }
    // https://en.wikipedia.org/wiki/Quartic_function#Solving_a_quartic_equation
    let p = (8.0 * coefficients[4] * coefficients[2] - 3.0 * coefficients[3].powi(2)) / (8.0 * coefficients[4].powi(2));
    let q = (coefficients[3].powi(3) - 4.0 * coefficients[4] * coefficients[3] * coefficients[2] + 8.0 * coefficients[4].powi(2) * coefficients[1])
        / (8.0 * coefficients[4].powi(3));
    let d = [
        coefficients[2].powi(2) - 3.0 * coefficients[3] * coefficients[1] + 12.0 * coefficients[4] * coefficients[0],
        2.0 * coefficients[2].powi(3) - 9.0 * coefficients[3] * coefficients[2] * coefficients[1]
            + 27.0 * coefficients[3].powi(2) * coefficients[0]
            + 27.0 * coefficients[4] * coefficients[1].powi(2)
            - 72.0 * coefficients[4] * coefficients[2] * coefficients[0],
    ];
    let discriminant = d[1].powi(2) - 4.0 * d[0].powi(3);
    let c = Scalar::new(discriminant).sqrt();
    let c = ((c + ComplexNumber::new(if c.real() + d[1] == 0.0 { -d[1] } else { d[1] }, 0.0)) * Scalar::new(0.5)).powf(1.0 / 3.0);
    let e = ((c + ComplexNumber::new(d[0], 0.0) / c) / Scalar::new(3.0 * coefficients[4]) - ComplexNumber::new(p * 2.0 / 3.0, 0.0)).powf(0.5)
        * Scalar::new(0.5);
    let mut solutions = Vec::with_capacity(4);
    for i in 0..4 {
        let f = (e * e * Scalar::new(-4.0) - ComplexNumber::new(2.0 * p, 0.0) + ComplexNumber::new(if i & 2 == 0 { q } else { -q }, 0.0) / e)
            .powf(0.5)
            * Scalar::new(0.5);
        let g =
            ComplexNumber::new(-coefficients[3] / (4.0 * coefficients[4]), 0.0) + if i & 2 == 0 { -e } else { e } + if i & 1 == 0 { -f } else { f };
        solutions.push(Root::new(g.real(), g.imaginary(), 1.0));
    }
    (discriminant / -27.0, solutions)
}
