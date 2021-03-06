//! Solving polynomials up to degree 4

#![allow(clippy::many_single_char_names)]
use crate::{complex_number::ComplexNumber, error::ERROR_MARGIN};

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
    let q = ComplexNumber::<f32>::from_sqrt_real(discriminant);
    let mut solutions = Vec::with_capacity(3);
    for s in &[-q, q] {
        let numerator = *s - coefficients[1];
        solutions.push(Root::new(numerator.real, numerator.imag, 2.0 * coefficients[2]));
    }
    (discriminant, solutions)
}

const ROOTS_OF_UNITY_3: [ComplexNumber<f32>; 3] = [
    // 0.8660254037844386467637231707529361834714026269051903140279034897
    // ComplexNumber::from_polar(1.0, -120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 0.0),
    ComplexNumber {
        real: -0.5,
        imag: -0.8660254,
    },
    ComplexNumber { real: -0.5, imag: 0.8660254 },
    ComplexNumber { real: 1.0, imag: 0.0 },
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
    let d = glam::vec2(
        coefficients[2].powi(2) - 3.0 * coefficients[3] * coefficients[1],
        2.0 * coefficients[2].powi(3) - 9.0 * coefficients[3] * coefficients[2] * coefficients[1] + 27.0 * coefficients[3].powi(2) * coefficients[0],
    );
    let mut solutions = Vec::with_capacity(3);
    let discriminant = d[1].powi(2) - 4.0 * d[0].powi(3);
    let c = ComplexNumber::<f32>::from_sqrt_real(discriminant);
    let c = ((c + if c.real + d[1] == 0.0 { -d[1] } else { d[1] }) * 0.5).powf(1.0 / 3.0);
    for root_of_unity in &ROOTS_OF_UNITY_3 {
        let ci = c * *root_of_unity;
        let denominator = ci * (3.0 * coefficients[3]);
        let numerator = (ci * -coefficients[2] - ci * ci - d[0]) * denominator.conjugate();
        solutions.push(Root::new(numerator.real, numerator.imag, denominator.squared_abs()));
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
    let c = ComplexNumber::<f32>::from_sqrt_real(discriminant);
    let c = ((c + if c.real + d[1] == 0.0 { -d[1] } else { d[1] }) * 0.5).powf(1.0 / 3.0);
    let e = ((c + ComplexNumber::<f32>::from(d[0]) / c) / (3.0 * coefficients[4]) - p * 2.0 / 3.0).powf(0.5) * 0.5;
    let mut solutions = Vec::with_capacity(4);
    for i in 0..4 {
        let f =
            (e * e * -4.0 - ComplexNumber::<f32>::from(2.0 * p) + ComplexNumber::<f32>::from(if i & 2 == 0 { q } else { -q }) / e).powf(0.5) * 0.5;
        let g = ComplexNumber::<f32>::from(-coefficients[3] / (4.0 * coefficients[4]))
            + if i & 2 == 0 { -e } else { e }
            + if i & 1 == 0 { -f } else { f };
        solutions.push(Root::new(g.real, g.imag, 1.0));
    }
    (discriminant / -27.0, solutions)
}
