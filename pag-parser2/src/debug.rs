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

macro_rules! styled_write {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, "{}", $crate::debug::styled!($($arg)*))
    };
}

pub(crate) use styled;
pub(crate) use styled_write;
