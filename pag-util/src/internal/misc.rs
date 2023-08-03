use core::hint::unreachable_unchecked;

#[inline]
pub unsafe fn assume(cond: bool) {
    if !cond {
        unreachable_unchecked()
    }
}

#[inline]
#[rustfmt::skip]
pub fn lookahead_lut(input: &[u8], mut idx: usize, table: &[u8; 256], shift: usize) -> usize {
    let mask = 1 << shift;
    unsafe { assume(idx <= input.len()) };
    for chunk in input[idx..].chunks_exact(8) {
        if table[chunk[0] as usize] & mask == 0 {
        if table[chunk[1] as usize] & mask == 0 {
        if table[chunk[2] as usize] & mask == 0 {
        if table[chunk[3] as usize] & mask == 0 {
        if table[chunk[4] as usize] & mask == 0 {
        if table[chunk[5] as usize] & mask == 0 {
        if table[chunk[6] as usize] & mask == 0 {
        if table[chunk[7] as usize] & mask == 0 {
            idx += 8; continue; }
            idx += 7; return idx; }
            idx += 6; return idx; }
            idx += 5; return idx; }
            idx += 4; return idx; }
            idx += 3; return idx; }
            idx += 2; return idx; }
            idx += 1; return idx; }
        return idx;
    }
    unsafe { assume(idx <= input.len()) };
    idx + input[idx..]
        .iter()
        .position(|x| table[*x as usize] & mask > 0)
        .unwrap_or(input[idx..].len())
}
