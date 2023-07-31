use std::{collections::HashMap, rc::Rc};

use syn::Type;

use crate::utils::Appendix;

use super::{NFTable, Tag, translation::Translation, NormalForm, Action, semact::SemAct, AbstractType};

pub struct Normalized {
    nfs: NFTable,
    hints: HashMap<Tag, Rc<Type>>
}

impl Normalized {
    fn normalize(&mut self) {
        loop {
            let mut updates = Vec::new();
            for (target, nfs) in self.nfs.iter().map(|(k, v)| (k.clone(), v.clone())) {
                if !nfs.iter().any(|x| matches!(x, NormalForm::Unexpanded {.. })) {
                    continue;
                }
                let mut stepped = Vec::new();
                for i in nfs {
                    let NormalForm::Unexpanded{ actions, semact, ty } = i else {
                        stepped.push(i);
                        continue;
                    };
                    let first_subroutine = actions.iter().enumerate().find_map(|(index, act)| {
                        if let Action::Shift { tag, output } = act {
                            Some((index, tag, output.clone()))
                        } else {
                            None
                        }
                    });
                    match first_subroutine {
                        None => {
                            stepped.push(NormalForm::Empty{ actions, semact, ty });
                        }
                        Some((index, tag, output)) => {
                            let variable_nf = self.nfs.get(tag).cloned().expect("tag must have associated");
                            for k in variable_nf {
                                let head = actions[..index].iter().cloned();
                                let tail = actions[index + 1..].iter().cloned();
                                match k {
                                    NormalForm::Empty { actions: mut expanded_actions, semact: expanded_semact, ty } => {
                                        if !matches!(expanded_semact, SemAct::Gather) || matches!(ty.0, AbstractType::Tuple(..)) {
                                            let hint = self.hints.get(tag).cloned().map(Appendix);
                                            expanded_actions.push(Action::Reduce { semact: expanded_semact, hint, output: output.clone()});
                                        }
                                        let acts = head.chain(expanded_actions).chain(tail).collect();
                                        stepped.push(NormalForm::Unexpanded { actions: acts, semact: semact.clone(), ty: ty.clone() });
                                    }
                                    NormalForm::Unexpanded { actions: mut expanded_actions, semact: expanded_semact, .. } => {
                                        if !matches!(expanded_semact, SemAct::Gather) || matches!(ty.0, AbstractType::Tuple(..)) {
                                            let hint = self.hints.get(tag).cloned().map(Appendix);
                                            expanded_actions.push(Action::Reduce { semact: expanded_semact, hint, output: output.clone()});
                                        }
                                        let acts = head.chain(expanded_actions).chain(tail).collect();
                                        stepped.push(NormalForm::Unexpanded { actions: acts, semact: semact.clone(), ty: ty.clone() });
                                    }
                                    NormalForm::Sequence {
                                        token,
                                        actions: mut expanded_actions,
                                        semact: expanded_semact,
                                        ..
                                    } => {
                                        if !matches!(expanded_semact, SemAct::Gather) || matches!(ty.0, AbstractType::Tuple(..)) {
                                            let hint = self.hints.get(tag).cloned().map(Appendix);
                                            expanded_actions.push(Action::Reduce { semact: expanded_semact, hint, output: output.clone()});
                                        }
                                        let acts = head.chain(expanded_actions).chain(tail).collect();
                                        stepped.push(NormalForm::Sequence { token, actions: acts,  semact: semact.clone(), ty: ty.clone() });
                                    }
                                }
                            }
                        }
                    }
                }
                updates.push((target, stepped));
            }
            if updates.is_empty() {
                break;
            } else {
                for (k, v) in updates {
                    self.nfs.insert(k, v);
                }
            }
        }
    }
}

impl From<Translation> for Normalized {
    fn from(value: Translation) -> Self {
        let mut normalized = Self {
            nfs: value.semi_nfs,
            hints: value.hints
        };
        normalized.normalize();
        normalized
    }
}

#[cfg(test)]
mod test {
    use crate::{frontend::Ast, nf::normalization::Normalized};

    use super::Translation;

    #[test]
    fn sexpr() {
        let ast = syn::parse_str::<Ast>(
            r#"
            %entry = sexp;

            DIGIT  = '0'..'9';
            ALPHA  = 'a'..'z' | 'A'..'Z';
            LPAREN = "(";
            RPAREN = ")";
            ATOM   = ALPHA (ALPHA | DIGIT)*;
            %skip  = (" " | "\t" | "\n" | "\r")+;

            compound: SExp = LPAREN sexp+[sexp:Vec<_>] RPAREN { SExp::Compound(sexp) };
            atom    : SExp = ATOM[atom] { SExp::Atom(atom) };
            sexp    : SExp = compound
                           | atom;
            "#,
        )
        .unwrap();
        #[cfg(feature = "debug")]
        {
            let translation = Translation::from(&ast);
            println!("{}", translation.semi_nfs);
            let normalized = Normalized::from(translation);
            println!("{}", normalized.nfs);
        }
        #[cfg(not(feature = "debug"))]
        {
            let translation = Translation::from(&ast);
            let _ = Normalized::from(translation);
        }
    }
}