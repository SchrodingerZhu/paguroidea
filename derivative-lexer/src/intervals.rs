use smallvec::SmallVec;
use std::fmt::{Display, Formatter};

// A closed interval of u32s.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct ClosedInterval(pub u32, pub u32);

impl Display for ClosedInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (char::from_u32(self.0), char::from_u32(self.1)) {
            (Some(start), Some(end)) if start == end => write!(f, "'{}'", start.escape_debug()),
            (Some(start), Some(end)) => {
                write!(f, "'{}'..'{}'", start.escape_debug(), end.escape_debug())
            }
            _ => write!(f, "'\\u{{{:6X}}}'-'\\u{{{:6X}}}'", self.0, self.1),
        }
    }
}

#[macro_export]
macro_rules! interval {
    ($start:expr, $end:expr) => {
        $crate::intervals::ClosedInterval::new($start as u32, $end as u32)
    };
}

#[macro_export]
macro_rules! intervals {
    ($(($start:expr, $end:expr)),*) => {
        $crate::intervals::Intervals::new(vec![$($crate::intervals::ClosedInterval::new($start as u32, $end as u32)),*].into_iter())
    };
}

impl ClosedInterval {
    pub fn new(start: u32, end: u32) -> Self {
        Self(start, end)
    }
    // Check if two intervals overlap.
    pub fn overlaps(&self, other: &Self) -> bool {
        self.0 <= other.1 && other.0 <= self.1 || other.0 <= self.1 && self.0 <= other.1
    }
    // Check if two intervals are consecutive.
    pub fn is_consecutive(&self, other: &Self) -> bool {
        self.1 + 1 == other.0 || other.1 + 1 == self.0
    }
    // Merge two intervals.
    pub fn merge(&self, other: &Self) -> Self {
        debug_assert!(self.overlaps(other) || self.is_consecutive(other));
        Self::new(self.0.min(other.0), self.1.max(other.1))
    }
    pub fn intersection(&self, other: &Self) -> Self {
        debug_assert!(self.overlaps(other));
        Self::new(self.0.max(other.0), self.1.min(other.1))
    }
    pub fn contains(&self, other: &Self) -> bool {
        self.0 <= other.0 && other.1 <= self.1
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Intervals(SmallVec<[ClosedInterval; 2]>);

impl Intervals {
    pub fn representative(&self) -> u32 {
        self.0[0].0
    }
    pub fn new<I>(mut data: I) -> Option<Self>
    where
        I: Iterator<Item = ClosedInterval>,
    {
        match data.next() {
            None => None,
            Some(first) => Some(data.fold(Self([first].into_iter().collect()), |acc, x| {
                let temp = Self([x].into_iter().collect());
                acc.union(&temp)
            })),
        }
    }
    // it is okay is contains non-unicode code points; they will never be read anyway.
    pub fn complement(&self) -> Self {
        let mut current = 0u32;
        let mut result = SmallVec::new();
        for i in self.0.iter() {
            if current < i.0 as u32 {
                result.push(ClosedInterval::new(current, i.0 - 1));
            }
            current = i.1 + 1;
        }
        if current <= 0x10FFFF {
            result.push(ClosedInterval::new(current, 0x10FFFF));
        }
        Self(result)
    }

    pub fn contains(&self, target: u32) -> bool {
        match self.0.binary_search_by_key(&target, |x| x.0) {
            Ok(_) => true,
            Err(idx) => {
                if idx == 0 {
                    false
                } else {
                    let idx = idx - 1;
                    self.0[idx].1 >= target
                }
            }
        }
    }

    pub fn is_complement(&self, other: &Self) -> bool {
        self.complement() == *other
    }

    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let mut result: Option<Self> = None;
        for i in self.0.iter().copied() {
            'inner: for j in other.0.iter().copied() {
                if i.overlaps(&j) {
                    let temp = i.intersection(&j);
                    result = match result {
                        None => Self::new([temp].into_iter()),
                        Some(x) => unsafe {
                            Some(x.union(&Self::new([temp].into_iter()).unwrap_unchecked()))
                        },
                    };
                } else {
                    if j.0 > i.1 {
                        break 'inner;
                    }
                }
            }
        }
        result
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut result = SmallVec::new();
        let mut i = self.0.iter().copied();
        let mut j = other.0.iter().copied();
        let mut x = i.next();
        let mut y = j.next();
        let mut current = None;
        loop {
            if current.is_none() {
                current = match (x, y) {
                    (Some(x), Some(y)) if x.0 < y.0 => Some(x),
                    (_, Some(y)) => Some(y),
                    (Some(x), _) => Some(x),
                    _ => break,
                };
            }
            current = {
                let current = unsafe { current.unwrap_unchecked() };
                if let Some(ix) = x &&
                    (current.overlaps(&ix) || current.is_consecutive(&ix)) {
                    x = i.next();
                    Some(current.merge(&ix))
                } else if let Some(iy) = y &&
                    (current.overlaps(&iy) || current.is_consecutive(&iy)) {
                    y = j.next();
                    Some(current.merge(&iy))
                } else {
                    result.push(current);
                    None
                }
            }
        }
        Self(result)
    }
}

impl Display for Intervals {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
            for i in iter {
                write!(f, " | {}", i)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn basic_format() {
        let interval = super::ClosedInterval::new(0x41, 0x5A);
        assert_eq!(format!("{}", interval), "'A'..'Z'");
        let interval = super::ClosedInterval::new(0x41, 0x7A);
        assert_eq!(format!("{}", interval), "'A'..'z'");
        let interval = super::ClosedInterval::new(0x41, 0x7B);
        assert_eq!(format!("{}", interval), "'A'..'{'");
        // whitespace
        let interval = super::ClosedInterval::new('\t' as _, '\t' as _);
        assert_eq!(format!("{}", interval), "'\\t'..'\\t'");
        // unicode
        let interval = super::ClosedInterval::new(0x1F600, 0x1F600);
        assert_eq!(format!("{}", interval), "'😀'..'😀'");
    }
    #[test]
    fn intervals_format() {
        let intervals = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        println!("{}", intervals);
    }
    #[test]
    fn union() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        assert_eq!(x.union(&x), x);
        let y = intervals!(('!', '7')).unwrap();
        assert_eq!(
            intervals!(('!', '9'), ('A', 'Z'), ('a', 'z')).unwrap(),
            x.union(&y)
        );
        let z = intervals!(('!', '7'), ('C', 'e')).unwrap();
        assert_eq!(intervals!(('!', '9'), ('A', 'z')).unwrap(), x.union(&z));
    }
    #[test]
    fn complement() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        let y = intervals!((0, 47), (58, 64), (91, 96), (123, 0x10FFFF)).unwrap();
        assert_eq!(x.complement(), y);
        let z = intervals!(('\0', '7')).unwrap();
        assert_eq!(z.complement(), intervals!(('8', 0x10FFFF)).unwrap());
        assert_eq!(x.complement().complement(), x);
        assert_eq!(x.union(&x.complement()), intervals!((0, 0x10FFFF)).unwrap());
    }
    #[test]
    fn intersection() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        let z = intervals!(('\0', '7')).unwrap();
        assert_eq!(x.intersection(&z), Some(intervals!(('0', '7')).unwrap()));
        assert!(x.intersection(&x.complement()).is_none());
        assert_eq!(
            x.intersection(&intervals!((0, 0x10FFFF)).unwrap()).unwrap(),
            x
        );
        let a = intervals!(('E', 'c')).unwrap();
        assert_eq!(
            x.intersection(&a),
            Some(intervals!(('E', 'Z'), ('a', 'c')).unwrap())
        );
    }
}
