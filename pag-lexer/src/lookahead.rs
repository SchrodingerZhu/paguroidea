// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::intervals::{byte_char, Interval, Intervals};
use crate::vector::{DfaState, DfaTable};
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

enum Kind {
    Positive,
    Negative,
}

fn generate_lut_routine(index: usize) -> TokenStream {
    let table = index / 8;
    let shift = index % 8;
    let bit = 1u8 << shift;
    quote! {
        idx = idx
            + input[idx..]
                .iter()
                .position(|x| GLOBAL_LUT[#table][*x as usize] & #bit > 0)
                .unwrap_or(input.len() - idx);
    }
}

fn byte_simd(byte: u8) -> TokenStream {
    let byte = byte_char(byte);
    quote! {
        data.simd_eq(u8x16::splat(#byte))
    }
}

fn range_simd(min: u8, max: u8) -> TokenStream {
    let min = byte_char(min);
    let max = byte_char(max);
    quote! {
        (data.simd_ge(u8x16::splat(#min)) & data.simd_le(u8x16::splat(#max)))
    }
}

fn generate_lookahead_routine(intervals: &Intervals, kind: Kind) -> TokenStream {
    let count_act = match kind {
        Kind::Positive => quote! { trailing_ones },
        Kind::Negative => quote! { trailing_zeros },
    };
    let idx_offset = intervals
        .iter()
        .map(|&Interval(l, r)| match l == r {
            true => byte_simd(l),
            false => range_simd(l, r),
        })
        .reduce(|acc, x| quote! { #acc | #x })
        .map(|x| {
            if cfg!(target_arch = "aarch64") {
                quote! {{
                    let mask : u128 = unsafe { core::mem::transmute(#x) };
                    mask.#count_act() / 8
                }}
            } else {
                quote! {
                    (#x).to_bitmask().#count_act()
                }
            }
        });
    quote! {
        for i in input[idx..].array_chunks::<16>() {
            use core::simd::*;
            let data = u8x16::from_slice(i);
            let idx_offset = #idx_offset;
            idx += idx_offset as usize;
            if core::intrinsics::unlikely(idx_offset != 16) {
                break;
            }
        }
    }
}

fn estimated_cost(intervals: &Intervals) -> u32 {
    intervals
        .iter()
        .map(|Interval(l, r)| if l == r { 1 } else { 2 })
        .sum()
}

#[derive(Default)]
pub struct LoopOptimizer {
    global_lut: Vec<[u8; 256]>,
    assigned: HashMap<Intervals, usize>,
}

impl LoopOptimizer {
    pub fn new() -> Self {
        Self {
            global_lut: Vec::new(),
            assigned: HashMap::new(),
        }
    }

    fn assign_table(&mut self, negatives: &Intervals) -> usize {
        let assigned_table = self.assigned.len();
        match self.assigned.entry(negatives.clone()) {
            Entry::Occupied(x) => {
                return *x.get();
            }
            Entry::Vacant(x) => {
                x.insert(assigned_table);
            }
        };
        let table = assigned_table / 8;
        let offset = assigned_table % 8;
        if self.global_lut.len() <= table {
            self.global_lut.push([0; 256]);
        }
        for i in u8::MIN..=u8::MAX {
            if negatives.contains(i) {
                self.global_lut[table][i as usize] |= 1u8 << offset;
            }
        }
        assigned_table
    }

    pub fn generate_lut(&self) -> Option<TokenStream> {
        if self.assigned.is_empty() {
            return None;
        }
        let table_size = self.global_lut.len();
        let table = self.global_lut.iter().map(|x| quote!([#(#x,)*]));
        Some(quote! {
            const GLOBAL_LUT : [[u8; 256]; #table_size] = [ #(#table,)* ];
        })
    }

    pub fn generate_lookahead(&mut self, dfa: &DfaTable, state: &DfaState) -> Option<TokenStream> {
        let limit = 4;

        let positives = direct_self_loops(dfa, state)?;
        if estimated_cost(&positives) <= limit {
            return Some(generate_lookahead_routine(&positives, Kind::Positive));
        }

        let negatives = positives.complement()?;
        if estimated_cost(&negatives) <= limit {
            return Some(generate_lookahead_routine(&negatives, Kind::Negative));
        }

        let index = self.assign_table(&negatives);
        Some(generate_lut_routine(index))
    }
}

fn direct_self_loops(dfa: &DfaTable, state: &DfaState) -> Option<Intervals> {
    let mut intervals: Option<Intervals> = None;
    for (edge, target) in &dfa.get(state)?.transitions {
        if target == state {
            intervals = Some(intervals.map_or_else(|| edge.clone(), |x| x.union(edge)));
        }
    }
    intervals
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lookahead_codegen() {
        use crate::intervals;
        let positives = intervals!((b'0', b'9'), (b'0', b'9'), (b'A', b'F'));
        syn::parse2::<syn::Expr>(generate_lookahead_routine(&positives, Kind::Positive)).unwrap();
        syn::parse2::<syn::Expr>(generate_lookahead_routine(&positives, Kind::Negative)).unwrap();
    }
}
