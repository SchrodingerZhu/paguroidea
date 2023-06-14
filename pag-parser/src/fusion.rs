// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use pag_lexer::lookahead::LoopOptimizer;
use pag_lexer::vector::Vector;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    frontend::{lexical::LexerDatabase, syntax::Parser},
    nf::{Action, NormalForm, NormalForms, Tag},
    utilities::Symbol,
};

fn generate_tag_enum(parser: &Parser<'_, '_>) -> TokenStream {
    let active_rules = parser
        .bindings
        .iter()
        .filter(|(_, rule)| rule.active)
        .map(|(sym, _)| format_ident!("{}", sym.name()));
    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Tag {
            #(#active_rules,)*
        }
    }
}

fn generate_parse_tree() -> TokenStream {
    quote! {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct ParserTree<'a> {
            tag: Tag,
            src: &'a str,
            span: core::ops::Range<usize>,
            children: alloc::vec::Vec<Self>
        }

        impl <'a> ParserTree<'a> {
            pub fn new(tag: Tag, src: &'a str) -> Self {
                Self {
                    tag,
                    src,
                    span: 0..0,
                    children: alloc::vec::Vec::new(),
                }
            }
            pub fn len(&self) -> usize {
                self.span.len()
            }
            pub fn children(&self) -> &[Self] {
                &self.children
            }
            pub fn tag(&self) -> &Tag {
                &self.tag
            }
            pub fn as_slice(&self) -> &'a str {
                &self.src[self.span.clone()]
            }
            pub fn set_span(&mut self, span: core::ops::Range<usize>) {
                self.span = span;
            }
            pub fn add_child(&mut self, child: Self) {
                self.children.push(child);
            }
            pub fn next_level(mut self, tag: Tag, span: core::ops::Range<usize>) -> Self {
                self.set_span(span);
                let mut result = Self::new(tag, self.src);
                result.add_child(self);
                result
            }
        }
    }
}

fn generate_error() -> TokenStream {
    quote! {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct Error {
            pub active_rule: Tag,
            pub expecting: &'static [&'static str],
            pub offset: usize,
        }

        impl core::fmt::Display for Error {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let expect = match self.expecting {
                    [head] => head.to_string(),
                    [init @ .., last] => format!("{} or {last}", init.join(", ")),
                    _ => unsafe { std::intrinsics::unreachable() },
                };
                write!(
                    f,
                    "expecting {expect} for {:?} at offset {}",
                    self.active_rule, self.offset
                )
            }
        }

        impl std::error::Error for Error {}
    }
}

fn generate_inactive_to_active_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            let child = #target_function(src, cursor)?;
            cursor = child.span.end;
            parent.add_child(child);
        }
    }
}

fn generate_active_to_active_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            let child = #target_function(src, cursor)?;
            cursor = child.span.end;
            tree.add_child(child);
        }
    }
}

fn generate_active_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        cursor = #target_function(src, cursor, &mut tree)?;
    }
}

fn generate_inactive_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        cursor = #target_function(src, cursor, parent)?;
    }
}

fn generate_subtree_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        cursor = #target_function(src, cursor, &mut subtree)?;
    }
}

fn generate_subtree_to_active_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            let child = #target_function(src, cursor)?;
            cursor = child.span.end;
            subtree.add_child(child);
        }
    }
}

fn generate_empty_actions(active: bool, symbols: &[Symbol<'_>]) -> Vec<TokenStream> {
    symbols
        .iter()
        .map(|x| {
            let tag = format_ident!("{}", x.name());
            let target = if active {
                format_ident!("tree")
            } else {
                format_ident!("parent")
            };
            quote! {
                {
                    let mut subtree = ParserTree::new(Tag::#tag, src);
                    subtree.set_span(cursor..cursor);
                    #target.add_child(subtree);
                }
            }
        })
        .collect()
}

fn create_next_tree_indices<'src>(actions: &[Action<'src>]) -> HashMap<usize, Symbol<'src>> {
    let mut result = HashMap::new();
    let mut last_index = 0;
    for (idx, action) in actions.iter().enumerate() {
        if let Action::Summarize(sym) = action {
            result.insert(last_index, *sym);
            last_index = idx + 1;
        }
    }
    result
}

fn generate_children<'src>(
    current: &Tag<'src>,
    active: bool,
    parser: &Parser<'src, '_>,
    rules: &[&NormalForm<'src>],
) -> Vec<TokenStream> {
    rules
        .iter()
        .filter(|x| !matches!(x, NormalForm::Empty(..)))
        .map(|nf| {
            let NormalForm::Sequence { nonterminals, .. } = nf else { unreachable!() };

            let mut add_continue = false;
            let mut actions = Vec::new();
            let mut subtree = false;

            let next_tree_indices = create_next_tree_indices(nonterminals);
            if let Some(sym) = next_tree_indices.get(&0) {
                let tag = format_ident!("{}", sym.name());
                actions.push(quote! {
                    let mut subtree = ParserTree::new(Tag::#tag, src);
                });
                subtree = true;
            }

            for i in 0..nonterminals.len() {
                match &nonterminals[i] {
                    Action::Subroutine(routine) => match (subtree, parser.is_active(routine)) {
                        (false, false) => {
                            if active {
                                actions.push(generate_active_to_inactive_call(routine))
                            } else if current == routine && i == nonterminals.len() - 1 {
                                add_continue = true;
                            } else {
                                actions.push(generate_inactive_to_inactive_call(routine))
                            }
                        }
                        (false, true) => {
                            if active {
                                actions.push(generate_active_to_active_call(routine))
                            } else {
                                actions.push(generate_inactive_to_active_call(routine))
                            }
                        }
                        (true, false) => actions.push(generate_subtree_to_inactive_call(routine)),
                        (true, true) => actions.push(generate_subtree_to_active_call(routine)),
                    },
                    Action::Summarize(..) => match next_tree_indices.get(&(i + 1)) {
                        Some(sym) => {
                            let tag = format_ident!("{}", sym.name());
                            actions.push(quote! {
                                subtree = subtree.next_level(Tag::#tag, offset..cursor);
                            });
                        }
                        None => {
                            subtree = false;
                            if active {
                                actions.push(quote! {
                                    subtree.set_span(offset..cursor);
                                    tree.add_child(subtree);
                                });
                            } else {
                                actions.push(quote! {
                                    subtree.set_span(offset..cursor);
                                    parent.add_child(subtree);
                                });
                            }
                        }
                    },
                }
            }
            if add_continue {
                quote! {
                    {
                        cursor = idx;
                        #(#actions)*
                        offset = cursor;
                        continue 'parser;
                    }
                }
            } else {
                quote! {
                    {
                        cursor = idx;
                        #(#actions)*
                        break 'parser;
                    }
                }
            }
        })
        .collect()
}

fn generate_skip() -> TokenStream {
    quote! {
        {
            offset = idx;
            continue 'parser;
        }
    }
}

fn generate_expect<'src>(
    rules: &[&NormalForm<'src>],
    lexer_database: &LexerDatabase<'src>,
) -> TokenStream {
    let elements: Vec<&str> = rules
        .iter()
        .filter_map(|x| match x {
            NormalForm::Sequence { terminal, .. } => Some(terminal.name()),
            _ => None,
        })
        .chain(lexer_database.skip.map(|x| x.name()))
        .collect();
    quote! {
        const EXPECTING: &[&str] = &[#(#elements),*];
    }
}

fn generate_inactive_parser<'src>(
    tag: Tag<'src>,
    parser: &Parser<'src, '_>,
    lexer_database: &LexerDatabase<'src>,
    rules: &[&NormalForm<'src>],
    loop_optimizer: &mut LoopOptimizer,
) -> TokenStream {
    let tag_name = format!("{tag}");
    let parser_name = format_ident!("parse_{tag_name}");
    let expect = generate_expect(rules, lexer_database);

    let success_actions = generate_children(&tag, false, parser, rules)
        .into_iter()
        .chain(lexer_database.skip.map(|_| generate_skip()))
        .collect::<Vec<_>>();
    let failure_action = match rules.iter().find_map(|x| match x {
        NormalForm::Empty(e) => Some(e),
        _ => None,
    }) {
        Some(e) => {
            let actions = generate_empty_actions(false, e);
            quote! {
                {
                    #(#actions)*
                    break 'parser;
                }
            }
        }
        None => quote! {
            return Err(Error {
                active_rule: parent.tag,
                expecting: EXPECTING,
                offset,
            })
        },
    };

    let fused = fusion_lexer(rules, lexer_database).generate_dfa(
        &quote!(offset),
        loop_optimizer,
        &success_actions,
        &failure_action,
    );
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            mut offset: usize,
            parent: &mut ParserTree<'a>,
        ) -> Result<usize, Error> {
            #expect
            let mut cursor = offset;
            'parser: loop {
                let input = src.as_bytes();
                #fused
            }
            Ok(cursor)
        }
    }
}

fn generate_active_parser<'src>(
    tag: Tag<'src>,
    parser: &Parser<'src, '_>,
    lexer_database: &LexerDatabase<'src>,
    rules: &[&NormalForm<'src>],
    loop_optimizer: &mut LoopOptimizer,
) -> TokenStream {
    let tag_name = format!("{tag}");
    let tag_ident = format_ident!("{tag_name}");
    let parser_name = format_ident!("parse_{tag_name}");
    let expect = generate_expect(rules, lexer_database);

    let success_actions = generate_children(&tag, true, parser, rules)
        .into_iter()
        .chain(lexer_database.skip.map(|_| generate_skip()))
        .collect::<Vec<_>>();
    let failure_action = match rules.iter().find_map(|x| match x {
        NormalForm::Empty(e) => Some(e),
        _ => None,
    }) {
        Some(e) => {
            let actions = generate_empty_actions(true, e);
            quote! {
                {
                    #(#actions)*
                    break 'parser;
                }
            }
        }
        None => quote! {
                return Err(Error{
                    active_rule: tree.tag,
                    expecting: EXPECTING,
                    offset,
                })
        },
    };
    let fused = fusion_lexer(rules, lexer_database).generate_dfa(
        &quote!(offset),
        loop_optimizer,
        &success_actions,
        &failure_action,
    );
    quote! {
        fn #parser_name(
            src: &str,
            mut offset: usize,
        ) -> Result<ParserTree, Error> {
            #expect
            let mut tree = ParserTree::new(Tag::#tag_ident, src);
            let mut cursor = offset;
            'parser: loop {
                let input = src.as_bytes();
                #fused
            }
            tree.set_span(offset..cursor);
            Ok(tree)
        }
    }
}

pub fn fusion_parser<'src>(
    rules: &NormalForms<'src, '_>,
    parser: &Parser<'src, '_>,
) -> TokenStream {
    let tag = generate_tag_enum(parser);
    let tree = generate_parse_tree();
    let error = generate_error();
    let mut loop_optimizer = LoopOptimizer::new();
    let parsers = rules
        .entries
        .iter()
        .map(|(tag, rules)| {
            if parser.is_active(tag) {
                generate_active_parser(
                    *tag,
                    parser,
                    &parser.lexer_database,
                    rules,
                    &mut loop_optimizer,
                )
            } else {
                generate_inactive_parser(
                    *tag,
                    parser,
                    &parser.lexer_database,
                    rules,
                    &mut loop_optimizer,
                )
            }
        })
        .collect::<Vec<_>>()
        .into_iter();
    let lut = loop_optimizer.generate_lut().into_iter();
    quote! {
        extern crate alloc;
        #tag
        #tree
        #error
        #(#parsers)*
        #(#lut)*
    }
}

pub fn fusion_lexer<'src>(
    rules: &[&NormalForm<'src>],
    lexer_database: &LexerDatabase<'src>,
) -> Vector {
    Vector::new(
        rules
            .iter()
            .filter_map(|x| match x {
                NormalForm::Empty(..) => None,
                NormalForm::Unexpanded(..) => None,
                NormalForm::Sequence { terminal, .. } => lexer_database
                    .entries
                    .get(terminal)
                    .map(|x| x.rule.node.clone()),
            })
            .chain(
                lexer_database
                    .skip
                    .and_then(|x| lexer_database.entries.get(&x).map(|x| x.rule.node.clone())),
            ),
    )
}
