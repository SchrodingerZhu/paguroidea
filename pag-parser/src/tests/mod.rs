use ariadne::Source;
use strip_ansi_escapes::Writer;

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

fn write_error<S: AsRef<str>, N: AsRef<str>>(input: S, name: N) -> String {
    let mut buffer = Vec::<u8>::new();
    {
        let result = crate::generate_parser(input.as_ref()).unwrap_err();
        let reports = result.to_reports(name.as_ref());
        let mut cache = (name.as_ref(), Source::from(input.as_ref()));
        let mut writer = Writer::new(&mut buffer);
        for i in reports {
            i.write(&mut cache, &mut writer).unwrap();
        }
    }
    String::from_utf8(buffer).unwrap()
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
