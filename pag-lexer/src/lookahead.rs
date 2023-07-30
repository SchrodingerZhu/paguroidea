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
    quote! { idx = ::pag_util::lookahead_lut(input, idx, &GLOBAL_LUT[#table], #shift); }
}

#[cfg(not(target_arch = "aarch64"))]
fn generate_lookahead_routine(intervals: &Intervals, kind: Kind) -> TokenStream {
    let mask = intervals
        .iter()
        .map(|&Interval(l, r)| match l == r {
            true => {
                let l = byte_char(l);
                quote! { data.simd_eq(u8x16::splat(#l)) }
            }
            false => {
                let l = byte_char(l);
                let r = byte_char(r);
                quote! { data.simd_ge(u8x16::splat(#l)) & data.simd_le(u8x16::splat(#r)) }
            }
        })
        .reduce(|acc, x| quote! { #acc | #x })
        .unwrap();
    let count_act = match kind {
        Kind::Positive => quote! { trailing_ones },
        Kind::Negative => quote! { trailing_zeros },
    };
    let tail_match = match kind {
        Kind::Positive => quote! { matches!(input.get(idx), Some(#intervals)) },
        Kind::Negative => quote! { !matches!(input.get(idx), Some(#intervals) | None) },
    };
    quote! {
        'lookahead: {
            unsafe { ::pag_util::assume(idx <= input.len()) };
            for chunk in input[idx..].chunks_exact(16) {
                use core::simd::*;
                let data = u8x16::from_slice(chunk);
                let mask = #mask;
                let idx_offset = mask.to_bitmask().#count_act();
                idx += idx_offset as usize;
                if idx_offset != 16 {
                    break 'lookahead;
                }
            }
            while #tail_match {
                idx += 1;
            }
        }
    }
}

#[cfg(target_arch = "aarch64")]
fn generate_lookahead_routine(intervals: &Intervals, kind: Kind) -> TokenStream {
    let mask = intervals
        .iter()
        .map(|&Interval(l, r)| match l == r {
            true => {
                let l = byte_char(l);
                quote! { data.simd_eq(u8x16::splat(#l)) }
            }
            false => {
                let l = byte_char(l);
                let r = byte_char(r);
                quote! { data.simd_ge(u8x16::splat(#l)) & data.simd_le(u8x16::splat(#r)) }
            }
        })
        .reduce(|acc, x| quote! { #acc | #x })
        .unwrap();
    let count_act = match kind {
        Kind::Positive => quote! { trailing_ones },
        Kind::Negative => quote! { trailing_zeros },
    };
    quote! {
        unsafe { ::pag_util::assume(idx <= input.len()) };
        for chunk in input[idx..].chunks_exact(16) {
            use core::simd::*;
            let data = u8x16::from_slice(chunk);
            let mask = #mask;
            let mask = unsafe { core::mem::transmute::<_, u128>(mask) };
            let idx_offset = mask.#count_act() / 8;
            idx += idx_offset as usize;
            if idx_offset != 16 {
                break;
            }
        }
    }
}

fn estimated_cost(intervals: &Intervals) -> u32 {
    intervals
        .iter()
        .map(|Interval(l, r)| 1 + (l != r) as u32)
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
        for &Interval(l, r) in negatives.iter() {
            for i in l..=r {
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
        let negatives = positives.complement()?;
        let pos_cost = estimated_cost(&positives);
        let neg_cost = estimated_cost(&negatives);

        if pos_cost.min(neg_cost) > limit {
            let index = self.assign_table(&negatives);
            return Some(generate_lut_routine(index));
        }
        if pos_cost < neg_cost {
            Some(generate_lookahead_routine(&positives, Kind::Positive))
        } else {
            Some(generate_lookahead_routine(&negatives, Kind::Negative))
        }
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
