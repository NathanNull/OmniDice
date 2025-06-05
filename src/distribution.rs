use std::{
    collections::BTreeMap,
    ops::{Add, Div, Mul, Sub},
};

use ordered_float::OrderedFloat;

#[derive(Clone, Debug)]
pub struct Distribution {
    values: BTreeMap<i32, f32>,
}

impl Distribution {
    pub fn die_n(n: usize) -> Self {
        let mut values = BTreeMap::new();
        let prob = 1.0 / n as f32;
        for v in 1..=n as i32 {
            values.insert(v, prob);
        }
        Self { values }
    }

    pub fn n_die_m(n: usize, m: usize) -> Self {
        let die_m = Self::die_n(m);
        die_m.multiroll(n)
    }

    pub fn multiroll(&self, mut rolls: usize) -> Self {
        let mut pows = vec![self.clone()];
        while 2usize.pow(pows.len() as u32) <= rolls {
            let last = pows.last().unwrap();
            pows.push(last.clone() + last.clone());
        }
        let mut res = Self {
            values: BTreeMap::from_iter([(0, 1.)]),
        };
        let mut bit = 0;
        while rolls != 0 {
            if rolls & 1 == 1 {
                res = res + pows[bit].clone();
            }
            rolls >>= 1;
            bit += 1;
        }
        res
    }

    pub fn from_vec(vals: Vec<i32>) -> Self {
        let mut values = BTreeMap::new();
        let prob = 1.0 / vals.len() as f32;
        for v in vals {
            if let Some(val) = values.get_mut(&v) {
                *val += prob;
            } else {
                values.insert(v, prob);
            }
        }
        Self { values }
    }

    pub fn min(&self) -> i32 {
        self.values.first_key_value().map(|(k, _)| *k).unwrap_or(0)
    }

    pub fn max(&self) -> i32 {
        self.values.last_key_value().map(|(k, _)| *k).unwrap_or(0)
    }

    pub fn mean(&self) -> f32 {
        self.values
            .iter()
            .map(|(val, prob)| *val as f32 * prob)
            .sum::<f32>()
    }
}

const OUT_WIDTH: usize = 50;
impl std::fmt::Display for Distribution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_prob = self
            .values
            .iter()
            .map(|(_, prob)| OrderedFloat(*prob))
            .max()
            .map(|f| f.0)
            .unwrap_or(1.);
        let outs = self
            .values
            .iter()
            .map(|(val, prob)| (val.to_string(), prob))
            .collect::<Vec<_>>();
        for (val, prob) in outs {
            let filled_amt = (prob / max_prob * OUT_WIDTH as f32).round() as usize;
            let bar = str::repeat("▮", filled_amt);
            write!(f, "{val: <5}: {bar}\n")?;
        }
        Ok(())
    }
}

impl Add for Distribution {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut out = BTreeMap::new();
        for (val1, prob1) in &self.values {
            for (val2, prob2) in &rhs.values {
                *out.entry(val1 + val2).or_insert(0.0) += prob1 * prob2;
            }
        }
        let sum = out.values().sum::<f32>();
        for (_, prob) in &mut out {
            *prob /= sum;
        }
        if out.iter().any(|o| !o.1.is_finite()) {
            panic!("{self:?}+{rhs:?} caused NaN")
        }
        Self { values: out }
    }
}

impl Sub for Distribution {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut out = BTreeMap::new();
        for (val1, prob1) in &self.values {
            for (val2, prob2) in &rhs.values {
                *out.entry(val1 - val2).or_insert(0.0) += prob1 * prob2;
            }
        }
        let sum = out.values().sum::<f32>();
        for (_, prob) in &mut out {
            *prob /= sum;
        }
        if out.iter().any(|o| !o.1.is_finite()) {
            panic!("{self:?}+{rhs:?} caused NaN")
        }
        Self { values: out }
    }
}

impl Mul for Distribution {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut out = BTreeMap::new();
        for (val1, prob1) in &self.values {
            for (val2, prob2) in &rhs.values {
                *out.entry(val1 * val2).or_insert(0.0) += prob1 * prob2;
            }
        }
        let sum = out.values().sum::<f32>();
        for (_, prob) in &mut out {
            *prob /= sum;
        }
        if out.iter().any(|o| !o.1.is_finite()) {
            panic!("{self:?}+{rhs:?} caused NaN")
        }
        Self { values: out }
    }
}

impl Div for Distribution {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let mut out = BTreeMap::new();
        for (val1, prob1) in &self.values {
            for (val2, prob2) in &rhs.values {
                *out.entry(val1 / val2).or_insert(0.0) += prob1 * prob2;
            }
        }
        let sum = out.values().sum::<f32>();
        for (_, prob) in &mut out {
            *prob /= sum;
        }
        if out.iter().any(|o| !o.1.is_finite()) {
            panic!("{self:?}+{rhs:?} caused NaN")
        }
        Self { values: out }
    }
}

impl From<&i32> for Distribution {
    fn from(value: &i32) -> Self {
        Self::from_vec(vec![*value])
    }
}