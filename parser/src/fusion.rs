use std::{matches, rc::Rc};

use derivative_lexer::{normalization::normalize, regex_tree::RegexTree, vector::Vector};
use smallvec::SmallVec;

use crate::{frontend::lexical::LexerDatabase, nf::NormalForm, unreachable_branch};

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
            .filter_map(|x| lexer_database.entries.get(&x))
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
                    .get(&terminal)
                    .map(|x| x.rule.node.clone()),
            })
            .chain(
                lexer_database
                    .skip
                    .and_then(|x| lexer_database.entries.get(&x).map(|x| x.rule.node.clone())),
            ),
    )
}
