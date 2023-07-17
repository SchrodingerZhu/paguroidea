pub fn dbg_sort<T, U, F, K>(data: T, _f: F) -> impl Iterator<Item = U>
where
    T: IntoIterator<Item = U>,
    F: FnMut(&U) -> K,
    K: Ord,
{
    #[cfg(not(debug_assertions))]
    {
        data.into_iter()
    }
    #[cfg(debug_assertions)]
    {
        let mut vec = Vec::from_iter(data);
        vec.sort_unstable_by_key(_f);
        vec.into_iter()
    }
}
