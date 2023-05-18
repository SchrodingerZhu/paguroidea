use std::{matches, rc::Rc, vec};

use derivative_lexer::{normalization::normalize, regex_tree::RegexTree, vector::Vector};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    frontend::{lexical::LexerDatabase, syntax::Parser},
    nf::{NormalForm, NormalForms, Tag},
    unreachable_branch,
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
        }
    }
}
fn generate_inactive_to_active_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            let child = #target_function(src, cursor)?;
            cursor += child.len();
            parent.add_child(child);
        }
    }
}
fn generate_active_to_active_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            let child = #target_function(src, cursor)?;
            cursor += child.len();
            tree.add_child(child);
        }
    }
}

fn generate_active_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            cursor += #target_function(src, cursor, &mut tree)?;
        }
    }
}

fn generate_inactive_to_inactive_call(target: &Tag<'_>) -> TokenStream {
    let target_function = format_ident!("parse_{}", format!("{}", target));
    quote! {
        {
            cursor += #target_function(src, cursor, parent)?;
        }
    }
}

fn generate_children<'src>(
    active: bool,
    parser: &Parser<'src, '_>,
    rules: &[&NormalForm<'src>],
) -> Vec<TokenStream> {
    rules
        .iter()
        .enumerate()
        .map(|(index, subroutines)| {
            let subroutine_calls = match subroutines {
                NormalForm::Unexpanded(..) => unreachable_branch(),
                NormalForm::Empty => vec![],
                NormalForm::Sequence { nonterminals, .. } => nonterminals
                    .iter()
                    .map(|x| {
                        if !active {
                            if !x.is_versioned()
                                && parser
                                    .bindings
                                    .get(&x.symbol())
                                    .map(|x| x.active)
                                    .unwrap_or(false)
                            {
                                generate_inactive_to_active_call(x)
                            } else {
                                generate_inactive_to_inactive_call(x)
                            }
                        } else if !x.is_versioned()
                            && parser
                                .bindings
                                .get(&x.symbol())
                                .map(|x| x.active)
                                .unwrap_or(false)
                        {
                            generate_active_to_active_call(x)
                        } else {
                            generate_active_to_inactive_call(x)
                        }
                    })
                    .collect(),
            };
            quote! {
                Some((#index, shift)) => {
                    cursor += shift;
                    #(#subroutine_calls)*
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
    let parser_rules = generate_children(false, parser, rules).into_iter().chain(
        lexer_database
            .skip
            .map(|_| generate_skip(rules.len(), tag, false)),
    );
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
            parent: &mut ParserTree<'a>,
        ) -> Result<usize, ()> {
            #lexer
            let mut cursor = offset;
            match #lexer_name(&src[offset..]) {
                None => return Err(unimplemented!("error message is not implemented")),
                #(#parser_rules,)*
                _ => unreachable!("should not enter this branch"),
            }
            Ok(cursor - offset)
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
    let parser_rules = generate_children(true, parser, rules).into_iter().chain(
        lexer_database
            .skip
            .map(|_| generate_skip(rules.len(), tag, true)),
    );
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
        ) -> Result<ParserTree<'a>, ()> {
            #lexer
            let mut tree = ParserTree::new(Tag::#tag_ident, src);
            let mut cursor = offset;
            match #lexer_name(&src[offset..]) {
                None => return Err(unimplemented!("error message is not implemented")),
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
    let complement = if rules.iter().any(|x| matches!(x, NormalForm::Empty)) {
        rules
            .iter()
            .filter_map(|x| match x {
                NormalForm::Sequence { terminal, .. } => Some(terminal),
                _ => None,
            })
            .filter_map(|x| lexer_database.entries.get(x))
            .map(|x| x.rule.node.clone())
            .chain(
                lexer_database
                    .skip
                    .and_then(|x| lexer_database.entries.get(&x).map(|x| x.rule.node.clone())),
            )
            .fold(None, |acc, y| match acc {
                None => Some(y),
                Some(x) => Some(Rc::new(RegexTree::Union(x, y))),
            })
            .map(RegexTree::Complement)
            .map(Rc::new)
            .map(normalize)
    } else {
        None
    };
    Vector::new(
        rules
            .iter()
            .filter_map(|x| match x {
                NormalForm::Empty => complement.clone(),
                NormalForm::Unexpanded(..) => unreachable_branch(),
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
