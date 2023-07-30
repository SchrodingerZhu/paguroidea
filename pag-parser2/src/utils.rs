use std::ops::{Deref, DerefMut};

#[cfg(feature = "ansi-debug")]
macro_rules! styled {
    ($style:expr, $($arg:tt)*) => {
        {
            use nu_ansi_term::*;
            $style.paint(format!($($arg)*))
        }
    };
}
#[cfg(not(feature = "ansi-debug"))]
macro_rules! styled {
    ($style:expr, $($arg:tt)*) => {format!($($arg)*)};
}

#[cfg(feature = "debug")]
macro_rules! styled_write {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, "{}", $crate::utils::styled!($($arg)*))
    };
}

#[cfg(feature = "debug")]
pub(crate) use styled;

#[cfg(feature = "debug")]
pub(crate) use styled_write;

/// Appendix that does not count in equality/ordinality/hashing.
#[derive(Clone)]
pub struct Appendix<T>(pub T);

impl<T> Deref for Appendix<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Appendix<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> PartialEq for Appendix<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for Appendix<T> {}

impl<T> PartialOrd for Appendix<T> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

impl<T> Ord for Appendix<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T> std::hash::Hash for Appendix<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}
