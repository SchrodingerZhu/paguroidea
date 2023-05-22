// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{collections::HashMap, matches};

use pag_lexer::vector::Vector;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    frontend::{lexical::LexerDatabase, syntax::Parser},
    nf::{Action, NormalForm, NormalForms, Tag},
    unreachable_branch,
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
                let expectation = if self.expecting.len() == 1 {
                    self.expecting[0].to_string()
                } else {
                    format!(
                        "{} or {}",
                        self.expecting[0..self.expecting.len() - 1].join(", "),
                        self.expecting[self.expecting.len() - 1]
                    )
                };
                write!(
                    f,
                    "expecting {expectation} for {:?} at offset {}",
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
        {
            cursor = #target_function(src, cursor, &mut tree)?;
        }
    }
}

fn generate_inactive_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            cursor = #target_function(src, cursor, parent)?;
        }
    }
}

fn generate_subtree_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            cursor = #target_function(src, cursor, &mut subtree)?;
        }
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
    active: bool,
    parser: &Parser<'src, '_>,
    rules: &[&NormalForm<'src>],
) -> Vec<TokenStream> {
    rules
        .iter()
        .filter(|x| !matches!(x, NormalForm::Empty(..)))
        .enumerate()
        .map(|(index, subroutines)| {
            let actions = match subroutines {
                NormalForm::Unexpanded(..) => unreachable_branch(),
                NormalForm::Empty(symbols) => generate_empty_actions(active, symbols),
                NormalForm::Sequence { nonterminals, .. } => {
                    let mut result = Vec::new();
                    let mut subtree = false;
                    let next_tree_indices = create_next_tree_indices(nonterminals);
                    if let Some(sym) = next_tree_indices.get(&0) {
                        let tag = format_ident!("{}", sym.name());
                        result.push(quote! {
                            let mut subtree = ParserTree::new(Tag::#tag, src);
                        });
                        subtree = true;
                    }
                    for i in 0..nonterminals.len() {
                        match &nonterminals[i] {
                            Action::Subroutine(routine) => {
                                match (subtree, parser.is_active(routine)) {
                                    (false, false) => {
                                        if active {
                                            result.push(generate_active_to_inactive_call(routine))
                                        } else {
                                            result.push(generate_inactive_to_inactive_call(routine))
                                        }
                                    }
                                    (false, true) => {
                                        if active {
                                            result.push(generate_active_to_active_call(routine))
                                        } else {
                                            result.push(generate_inactive_to_active_call(routine))
                                        }
                                    }
                                    (true, false) => {
                                        result.push(generate_subtree_to_inactive_call(routine))
                                    }
                                    (true, true) => {
                                        result.push(generate_subtree_to_active_call(routine))
                                    }
                                }
                            }
                            Action::Summarize(..) => match next_tree_indices.get(&(i + 1)) {
                                Some(sym) => {
                                    let tag = format_ident!("{}", sym.name());
                                    result.push(quote! {
                                        subtree = subtree.next_level(Tag::#tag, offset..cursor);
                                    });
                                }
                                None => {
                                    subtree = false;
                                    if active {
                                        result.push(quote! {
                                            subtree.set_span(offset..cursor);
                                            tree.add_child(subtree);
                                        });
                                    } else {
                                        result.push(quote! {
                                            subtree.set_span(offset..cursor);
                                            parent.add_child(subtree);
                                        });
                                    }
                                }
                            },
                        }
                    }
                    result
                }
            };
            quote! {
                Some((#index, shift)) => {
                    cursor += shift;
                    #(#actions)*
                }
            }
        })
        .collect()
}

fn generate_skip(index: usize, tag: Tag<'_>, active: bool) -> TokenStream {
    let tag_name = format!("{}", tag);
    let parser_name = format_ident!("parse_{}", tag_name);
    if active {
        quote! {
            Some((#index, shift)) => {
                return #parser_name(src, offset + shift);
            }
        }
    } else {
        quote! {
            Some((#index, shift)) => {
                return #parser_name(src, offset + shift, parent);
            }
        }
    }
}

fn generate_expect<'src>(
    rules: &[&NormalForm<'src>],
    lexer_database: &LexerDatabase<'src>,
) -> TokenStream {
    let mut length1 = 0;
    let mut length2 = 0;
    let elements: Vec<&str> = rules
        .iter()
        .filter_map(|x| match x {
            NormalForm::Empty(..) => None,
            NormalForm::Unexpanded(..) => None,
            NormalForm::Sequence { terminal, .. } => {
                length1 += 1;
                Some(terminal.name())
            }
        })
        .chain(lexer_database.skip.and_then(|x| {
            length2 += 1;
            Some(x.name())
        }))
        .collect();
    let length = (length1 + length2) as usize;
    quote! {
        const EXPECTING: [&str; #length] = [#(#elements),*];
    }
}

fn generate_inactive_parser<'src>(
    tag: Tag<'src>,
    parser: &Parser<'src, '_>,
    lexer_database: &LexerDatabase<'src>,
    rules: &[&NormalForm<'src>],
) -> TokenStream {
    let tag_name = format!("{}", tag);
    let parser_name = format_ident!("parse_{}", tag_name);
    let expect = generate_expect(rules, lexer_database);
    let lexer = fusion_lexer(rules, lexer_database).generate_dfa(format!("lexer_{}", tag_name));
    let lexer_name = format_ident!("lexer_{}", tag_name);
    let parser_rules =
        generate_children(false, parser, rules)
            .into_iter()
            .chain(lexer_database.skip.map(|_| {
                generate_skip(
                    rules
                        .iter()
                        .filter(|x| !matches!(x, NormalForm::Empty(..)))
                        .count(),
                    tag,
                    false,
                )
            }));
    let none_action = match rules.iter().find_map(|x| match x {
        NormalForm::Empty(e) => Some(e),
        _ => None,
    }) {
        Some(e) => {
            let actions = generate_empty_actions(true, e);
            quote! {
                None => {
                    #(#actions)*
                }
            }
        }
        None => quote! {
            None => {
                return Err(Error {
                    active_rule: parent.tag,
                    expecting: &EXPECTING,
                    offset: offset,
                });
            }
        },
    };
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
            parent: &mut ParserTree<'a>,
        ) -> Result<usize, Error> {
            #expect
            #lexer
            let mut cursor = offset;
            match #lexer_name(&src[offset..]) {
                #none_action,
                #(#parser_rules,)*
                _ => unreachable!("should not enter this branch"),
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
) -> TokenStream {
    let tag_name = format!("{}", tag);
    let tag_ident = format_ident!("{}", tag_name);
    let parser_name = format_ident!("parse_{}", tag_name);
    let expect = generate_expect(rules, lexer_database);
    let lexer = fusion_lexer(rules, lexer_database).generate_dfa(format!("lexer_{}", tag_name));
    let lexer_name = format_ident!("lexer_{}", tag_name);
    let parser_rules =
        generate_children(true, parser, rules)
            .into_iter()
            .chain(lexer_database.skip.map(|_| {
                generate_skip(
                    rules
                        .iter()
                        .filter(|x| !matches!(x, NormalForm::Empty(..)))
                        .count(),
                    tag,
                    true,
                )
            }));
    let none_action = match rules.iter().find_map(|x| match x {
        NormalForm::Empty(e) => Some(e),
        _ => None,
    }) {
        Some(e) => {
            let actions = generate_empty_actions(true, e);
            quote! {
                None => {
                    #(#actions)*
                }
            }
        }
        None => quote! {
            None => {
                return Err(Error{
                    active_rule: *(&tree.tag),
                    expecting: &EXPECTING,
                    offset: offset,
                });
            }
        },
    };
    let modifier = if parser.entrypoint == tag.symbol() {
        Some(quote!(pub))
    } else {
        None
    }
    .into_iter();
    quote! {
        #(#modifier)* fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
        ) -> Result<ParserTree<'a>, Error> {
            #expect
            #lexer
            let mut tree = ParserTree::new(Tag::#tag_ident, src);
            let mut cursor = offset;
            match #lexer_name(&src[offset..]) {
                #none_action,
                #(#parser_rules,)*
                _ => unreachable!("should not enter this branch"),
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
    let parsers = rules.entries.iter().map(|(tag, rules)| {
        if !tag.is_versioned()
            && parser
                .bindings
                .get(&tag.symbol())
                .map(|x| x.active)
                .unwrap_or(false)
        {
            generate_active_parser(*tag, parser, &parser.lexer_database, rules)
        } else {
            generate_inactive_parser(*tag, parser, &parser.lexer_database, rules)
        }
    });
    quote! {
        extern crate alloc;
        #tag
        #tree
        #error
        #(#parsers)*
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
