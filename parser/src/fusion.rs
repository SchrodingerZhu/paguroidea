/*
MPL 2.0 License applies to this file with exceptions specified in 
the Apache License notice below.

Copyright (C) 2023 Paguroidea Developpers

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at <https://mozilla.org/MPL/2.0/>.
*/

/*
All quoted content inside this file, together with the generated code 
(distributed either as token streams or in the file form) is licensed 
under the Apache License, Version 2.0.

Copyright 2023 Paguroidea Developpers

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

use std::{collections::HashMap, matches};

use derivative_lexer::vector::Vector;
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

fn generate_inactive_parser<'src>(
    tag: Tag<'src>,
    parser: &Parser<'src, '_>,
    lexer_database: &LexerDatabase<'src>,
    rules: &[&NormalForm<'src>],
) -> TokenStream {
    let tag_name = format!("{}", tag);
    let parser_name = format_ident!("parse_{}", tag_name);
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
                unimplemented!("error message is not implemented");
            }
        },
    };
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
            parent: &mut ParserTree<'a>,
        ) -> Result<usize, ()> {
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
                unimplemented!("error message is not implemented");
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
        ) -> Result<ParserTree<'a>, ()> {
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
