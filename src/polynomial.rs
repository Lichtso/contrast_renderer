use crate::{complex_number::ComplexNumber, error::ERROR_MARGIN};

/// Finds the discreminant and root of: `0 = coefficients[1] * x + coefficients[0]`.
pub fn solve_linear(coefficients: [f32; 2]) -> (f32, Vec<glam::Vec2>) {
    if coefficients[1].abs() <= ERROR_MARGIN {
        (0.0, vec![])
    } else {
        (1.0, vec![glam::vec2(-coefficients[0], coefficients[1])])
    }
}

/// Finds the discreminant and roots of: `0 = coefficients[2] * x.powi(2) + coefficients[1] * x + coefficients[0]`.
pub fn solve_quadratic(coefficients: [f32; 3]) -> (f32, Vec<glam::Vec2>) {
    if coefficients[2].abs() <= ERROR_MARGIN {
        solve_linear([coefficients[0], coefficients[1]])
    } else {
        let discreminant = coefficients[1] * coefficients[1] - 4.0 * coefficients[2] * coefficients[0];
        let descreminant_root = discreminant.sqrt();
        (
            discreminant,
            vec![
                glam::vec2(-coefficients[1] - descreminant_root, 2.0 * coefficients[2]),
                glam::vec2(-coefficients[1] + descreminant_root, 2.0 * coefficients[2]),
            ],
        )
    }
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

/// Finds the discreminant and roots of: `0 = coefficients[3] * x.powi(3) + coefficients[2] * x.powi(2) + coefficients[1] * x + coefficients[0]`.
///
/// Also returns the index of the real root if there are two complex roots and one real root.
pub fn solve_cubic(coefficients: [f32; 4]) -> (f32, Vec<glam::Vec2>, usize) {
    if coefficients[3].abs() <= ERROR_MARGIN {
        let (discreminant, roots) = solve_quadratic([coefficients[0], coefficients[1], coefficients[2]]);
        return (discreminant, roots, 2);
    }
    // https://en.wikipedia.org/wiki/Cubic_equation
    let d = glam::vec2(
        coefficients[2] * coefficients[2] - 3.0 * coefficients[3] * coefficients[1],
        2.0 * coefficients[2] * coefficients[2] * coefficients[2] - 9.0 * coefficients[3] * coefficients[2] * coefficients[1]
            + 27.0 * coefficients[3] * coefficients[3] * coefficients[0],
    );
    let mut solutions = Vec::with_capacity(3);
    let discreminant = d[1] * d[1] - 4.0 * d[0] * d[0] * d[0];
    let mut c = ComplexNumber::from_sqrt_real(discreminant);
    c = ((c + if c.real + d[1] == 0.0 { -d[1] } else { d[1] }) * 0.5).pow_real(1.0 / 3.0);
    for root_of_unity in &ROOTS_OF_UNITY_3 {
        let ci = c * *root_of_unity;
        let numerator = ci * -coefficients[2] - ci * ci - d[0];
        let denominator = ci * (3.0 * coefficients[3]);
        solutions.push(glam::vec2((numerator * denominator.conjugate()).real, denominator.squared_abs()));
    }
    let real_root = (((std::f32::consts::PI - c.arg()) / (std::f32::consts::PI * 2.0 / 3.0)) as usize + 1) % 3;
    (discreminant, solutions, real_root)
}
