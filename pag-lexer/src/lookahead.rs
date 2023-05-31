use crate::intervals::Intervals;
use crate::vector::Vector;
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::HashMap;

#[derive(Copy, Clone)]
enum LookAheadEdge {
    Byte(u8),
    Range(u8, u8),
}

fn generate_lut_routine(index: usize) -> TokenStream {
    let table = index / 8;
    let shift = index % 8;
    quote! {
        idx = idx + input[idx..].iter().position(|x| (GLOBAL_LUT[#table][*x as usize] >> #shift) & 1 > 0)
            .unwrap_or(input.len() - idx);
    }
}

fn byte_simd(byte: u8) -> TokenStream {
    quote! {
        {
            let needles = u8x16::splat(#byte);
            data.simd_eq(needles)
        }
    }
}

fn range_simd(min: u8, max: u8) -> TokenStream {
    quote! {
        {
            let needles_min = u8x16::splat(#min);
            let needles_max = u8x16::splat(#max);
            let cmp_min = data.simd_ge(needles_min);
            let cmp_max = data.simd_le(needles_max);
            cmp_min & cmp_max
        }
    }
}

fn generate_positive_lookaheads(edges: &[LookAheadEdge]) -> TokenStream {
    let offsets = edges
        .iter()
        .map(|x| match x {
            LookAheadEdge::Byte(x) => byte_simd(*x),
            LookAheadEdge::Range(min, max) => range_simd(*min, *max),
        })
        .map(|x| {
            if cfg!(target_arch = "aarch64") {
                quote! {
                    {
                        let mask : u128 = unsafe { core::mem::transmute(#x) };
                        mask.trailing_ones() / 8
                    }
                }
            } else {
                quote! {
                    {
                        #x.to_bitmask().trailing_ones()
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    let max_offset = offsets[1..]
        .iter()
        .fold(offsets[0].clone(), |acc, x| quote!(#acc.max(#x)));
    quote! {
        for i in input[idx..].array_chunks::<16>() {
            use core::simd::*;
            let data = u8x16::from_slice(i);
            let max_offset = #max_offset;
            idx += max_offset as usize;
            if core::intrinsics::unlikely(max_offset != 16) {
                break;
            }
        }
    }
}

fn generate_negative_lookaheads(edges: &[LookAheadEdge]) -> TokenStream {
    let offsets = edges
        .iter()
        .map(|x| match x {
            LookAheadEdge::Byte(x) => byte_simd(*x),
            LookAheadEdge::Range(min, max) => range_simd(*min, *max),
        })
        .map(|x| {
            if cfg!(target_arch = "aarch64") {
                quote! {
                    {
                        let mask : u128 = unsafe { core::mem::transmute(#x) };
                        mask.trailing_zeros() / 8
                    }
                }
            } else {
                quote! {
                    {
                        #x.to_bitmask().trailing_zeros()
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    let min_offset = offsets[1..]
        .iter()
        .fold(offsets[0].clone(), |acc, x| quote!(#acc.min(#x)));
    quote! {
        for i in input[idx..].array_chunks::<16>() {
            use core::simd::*;
            let data = u8x16::from_slice(i);
            let min_offset = #min_offset;
            idx += min_offset as usize;
            if core::intrinsics::unlikely(min_offset != 16) {
                break;
            }
        }
    }
}

#[derive(Default)]
pub struct LoopOptimizer {
    global_lut: Vec<[u8; 256]>,
    assigned_table: usize,
}

fn convert_interval_to_edges(intervals: &Intervals) -> Vec<LookAheadEdge> {
    let mut processed = Vec::new();
    let mut current = None;
    for i in u8::MIN..=u8::MAX {
        if intervals.contains(i) {
            match current {
                None => {
                    current = Some(LookAheadEdge::Byte(i));
                }
                Some(LookAheadEdge::Byte(j)) if j + 1 != i => {
                    processed.push(LookAheadEdge::Byte(j));
                    current = Some(LookAheadEdge::Byte(i));
                }
                Some(LookAheadEdge::Byte(j)) => {
                    current = Some(LookAheadEdge::Range(j, i));
                }
                Some(LookAheadEdge::Range(a, b)) if b + 1 != i => {
                    processed.push(LookAheadEdge::Range(a, b));
                    current = Some(LookAheadEdge::Byte(i));
                }
                Some(LookAheadEdge::Range(a, _)) => {
                    current = Some(LookAheadEdge::Range(a, i));
                }
            }
        }
    }
    processed.extend(current);
    processed
}

impl LoopOptimizer {
    pub fn new() -> Self {
        Self {
            global_lut: Vec::new(),
            assigned_table: 0,
        }
    }
    fn assign_table(&mut self, negatives: &Intervals) -> usize {
        let table = self.assigned_table / 8;
        let offset = self.assigned_table % 8;
        self.assigned_table += 1;
        if self.global_lut.len() <= table {
            self.global_lut.push([0; 256]);
        }
        for i in u8::MIN..=u8::MAX {
            if negatives.contains(i) {
                self.global_lut[table][i as usize] |= 1u8 << offset;
            }
        }
        table * 8 + offset
    }
    pub fn generate_lut(&self) -> Option<TokenStream> {
        if self.assigned_table == 0 {
            return None;
        }
        let table_size = self.global_lut.len();
        let table = self.global_lut.iter().map(|x| quote!([#(#x,)*]));
        Some(quote! {
            const GLOBAL_LUT : [[u8;256]; #table_size] = [ #(#table,)* ];
        })
    }
    pub fn generate_lookahead(&mut self, dfa: &DFATable, state: &Vector) -> Option<TokenStream> {
        let limit = if cfg!(target_arch = "aarch64") { 2 } else { 4 };
        if let Some(intervals) = direct_self_loops(dfa, state) {
            let positives = convert_interval_to_edges(&intervals);
            if positives.len() <= limit {
                return Some(generate_positive_lookaheads(&positives));
            }
            if let Some(intervals) = intervals.complement() {
                let negatives = convert_interval_to_edges(&intervals);
                if negatives.len() <= limit {
                    return Some(generate_negative_lookaheads(&negatives));
                }
                let index = self.assign_table(&intervals);
                return Some(generate_lut_routine(index));
            }
        }
        None
    }
}

type DFATable = HashMap<Vector, Vec<(Intervals, Vector)>>;

fn direct_self_loops(dfa: &DFATable, state: &Vector) -> Option<Intervals> {
    let mut intervals = None;
    if let Some(transitions) = dfa.get(state) {
        for (edge, target) in transitions {
            if target == state {
                intervals =
                    Some(intervals.map_or_else(|| edge.clone(), |x: Intervals| x.union(edge)));
            }
        }
    }
    intervals
}

#[test]
fn test() {
    let lookaheads = [
        LookAheadEdge::Range(b'0', b'9'),
        LookAheadEdge::Range(b'a', b'f'),
        LookAheadEdge::Range(b'A', b'F'),
    ];
    println!("{}", generate_positive_lookaheads(&lookaheads));
}
