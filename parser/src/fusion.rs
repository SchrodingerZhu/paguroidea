use std::{matches, rc::Rc};

use derivative_lexer::{normalization::normalize, regex_tree::RegexTree, vector::Vector};
use proc_macro2::TokenStream;
use smallvec::SmallVec;
use quote::{quote, format_ident};

use crate::{frontend::{lexical::LexerDatabase, syntax::Parser}, nf::{NormalForm, Tag}, unreachable_branch};

fn generate_tag_enum<'src, 'a>(
    parser: &Parser<'src, 'a>,
) -> TokenStream {
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
    quote!{
        pub struct ParserTree<'a> {
            tag: Tag,
            src: &'a str,
            span: core::ops::Range<usize>,
            children: alloc::vec::Vec<Self>
        }
        
        impl <'a> ParserTree<'a> {
            pub fn new(tag: Tag, src: &'a str, span: core::ops::Range<usize>) -> Self {
                Self {
                    tag,
                    src,
                    span,
                    children: alloc::vec::Vec::new(),
                }
            }
            pub fn children(&self) -> &[Self] {
                &self.children
            }
            pub fn tag(&self) -> &Tag {
                &self.tag
            }
            pub fn add_child(&mut self, child: Self) {
                self.children.push(child);
            }
        }
    }
}

fn generate_inactive_parser<'src>(
    tag: Tag<'src>,
    lexer_database: &LexerDatabase<'src>,
    rules: &[&NormalForm<'src>],
)  -> TokenStream {
    let tag_name = format!("{}", tag);
    let parser_name = format_ident!("parse_{}", tag_name);
    let lexer = fusion_lexer(rules, lexer_database).generate_dfa(format!("lexer_{}", tag_name));
    let lexer_name = format_ident!("lexer_{}", tag_name);
    quote! {
        fn #parser_name<'a>(
            src: &'a str,
            offset: usize,
            parent: &mut ParserTree<'a>,
        ) -> Result<usize, ()> {
            #lexer
            match #lexer_name(&src[offset..]) {
                None => Err(unimplemented!("error message is not implemented")),
                #(#parser_rules,)* 
                _ => unreachable!("should not enter this branch"),
            }
        } 
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
