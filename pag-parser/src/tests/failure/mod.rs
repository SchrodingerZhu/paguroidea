// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::tests::write_error;

macro_rules! expect_error {
    ( $path:expr, $name:ident, $expect:expr ) => {
        #[test]
        fn $name() {
            let input = include_str!($path);
            let errors = write_error(input, $path);
            let lines1 = errors.trim().lines().map(str::trim_end).collect::<Vec<_>>();
            let lines2 = $expect
                .trim()
                .lines()
                .map(str::trim_end)
                .collect::<Vec<_>>();
            assert_eq!(lines1, lines2, "\n\n{errors}");
        }
    };
}

expect_error!(
    "err_nullable_token.pag",
    err_nullable_token,
    r#"
Error: Nullable token detected
   ╭─[err_nullable_token.pag:2:5]
   │
 2 │     A     = 'a'*;
   │     ┬
   │     ╰── token A is nullable
───╯
"#
);

expect_error!(
    "err_unguarded_fixpoint.pag",
    err_unguarded_fixpoint,
    r#"
Error: Unguarded fixpoint
   ╭─[err_unguarded_fixpoint.pag:6:19]
   │
 6 │     active test = test ~ A;
   │                   ────┬───
   │                       ╰───── fixpoint rule test is not guarded -- your grammar is left-recursive
───╯
"#
);

expect_error!(
    "err_alternation_ambiguity.pag",
    err_alternation_ambiguity,
    r#"
Error: When type checking an alternation of rules, the following rules are ambiguous
   ╭─[err_alternation_ambiguity.pag:6:19]
   │
 6 │     active test = A+ | A ~ test;
   │                   ─┬   ────┬───
   │                    ╰───────────── type info for left-hand side: nullable: false, first set:  {A}, follow set:  {A}
   │                            │
   │                            ╰───── type info for right-hand side: nullable: false, first set: {A}, follow set: {}
───╯
"#
);

expect_error!(
    "err_sequence_ambiguity.pag",
    err_sequence_ambiguity,
    r#"
Error: When type checking a sequence of rules, the following rules are ambiguous
   ╭─[err_sequence_ambiguity.pag:6:19]
   │
 6 │     active test = A+ ~ A;
   │                   ─┬   ┬
   │                    ╰────── type info for left-hand side: nullable: false, first set: {A}, follow set: {A}
   │                        │
   │                        ╰── type info for right-hand side: nullable: false, first set: {A}, follow set: {}
───╯
"#
);

expect_error!(
    "err_null_sequence_ambiguity.pag",
    err_null_sequence_ambiguity,
    r#"
Error: When type checking a sequence of rules, the following rules are ambiguous
   ╭─[err_null_sequence_ambiguity.pag:6:19]
   │
 6 │     active test = _ ~ A;
   │                   ┬   ┬
   │                   ╰────── type info for left-hand side: nullable: true, first set: {}, follow set: {}
   │                       │
   │                       ╰── type info for right-hand side: nullable: false, first set: {A}, follow set: {}
───╯
"#
);

expect_error!(
    "err_multiple_skips.pag",
    err_multiple_skips,
    r#"
Error: Skipping lexical rule is already defined
   ╭─[err_multiple_skips.pag:3:5]
   │
 2 │     skip = "SKIP";
   │     ──────┬──────
   │           ╰──────── first definition
 3 │     skip = "ANOTHER_SKIP";
   │     ──────────┬──────────
   │               ╰──────────── second definition
───╯
"#
);

expect_error!(
    "err_cyclic_token.pag",
    err_cyclic_token,
    r#"
Error: Cyclic lexical rule reference
   ╭─[err_cyclic_token.pag:2:15]
   │
 2 │     A = 'a' ~ A;
   │               ┬
   │               ╰── this reference causes cyclic dependency
───╯
"#
);

expect_error!(
    "err_undefined_token_in_lexer.pag",
    err_undefined_token_in_lexer,
    r#"
Error: Undefined lexical rule reference
   ╭─[err_undefined_token_in_lexer.pag:2:13]
   │
 2 │     A     = C;
   │             ┬
   │             ╰── lexcical rule C is undefined
───╯
"#
);

expect_error!(
    "err_undefined_token_in_parser.pag",
    err_undefined_token_in_parser,
    r#"
Error: Undefined lexical rule reference
   ╭─[err_undefined_token_in_parser.pag:6:19]
   │
 6 │     active test = AA;
   │                   ─┬
   │                    ╰── lexcical rule AA is undefined
───╯
"#
);

expect_error!(
    "err_undefined_grammar_rule.pag",
    err_undefined_grammar_rule,
    r#"
Error: Undefined parser rule reference
   ╭─[err_undefined_grammar_rule.pag:6:19]
   │
 6 │     active test = test2;
   │                   ──┬──
   │                     ╰──── parser rule test2 is undefined
───╯
"#
);

expect_error!(
    "err_multiple_definitions_in_lexer.pag",
    err_multiple_definitions_in_lexer,
    r#"
Error: Multiple definition of A
   ╭─[err_multiple_definitions_in_lexer.pag:3:5]
   │
 2 │     A     = '0';
   │     ┬
   │     ╰── first definition
 3 │     A     = '1';
   │     ┬
   │     ╰── second definition
───╯
"#
);

expect_error!(
    "err_multiple_definitions_in_parser.pag",
    err_multiple_definitions_in_parser,
    r#"
Error: Multiple definition of test
   ╭─[err_multiple_definitions_in_parser.pag:7:12]
   │
 6 │     active test = A;
   │            ──┬─
   │              ╰─── first definition
 7 │     active test = A;
   │            ──┬─
   │              ╰─── second definition
───╯
"#
);
