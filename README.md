<!--

-->
<h1 align="center">
<!--suppress CheckImageSize -->
<img src="https://raw.githubusercontent.com/SchrodingerZhu/paguroidea/main/.github/images/hermit-crab.png" alt="hermit-crab" width="50%" height="50%">
<br>
Paguroidea
</h1>

![GITHUB-BADGE](https://github.com/SchrodingerZhu/paguroidea/workflows/Build/badge.svg)

| Crate          | Status                                                         |
|----------------|----------------------------------------------------------------|
| `pag-lexer`    | ![crates.io](https://img.shields.io/crates/v/pag-lexer.svg)    |
| `pag-parser`   | ![crates.io](https://img.shields.io/crates/v/pag-parser.svg)   |
| `pag-compiler` | ![crates.io](https://img.shields.io/crates/v/pag-compiler.svg) |



A reimplementation of the Flap parser in Rust (with our own modifications applied)!

## 🚧 Under Construction 🚧
This project is still under its early-stage development. The grammar for Paguroidea is subject to change
(see [Issue #22](https://github.com/SchrodingerZhu/paguroidea/issues/22)). The parser generation is not thoroughly tested,
which may still shake some bugs out from time to time. There are also ongoing works to improvement the quality of the generated
code.

## Introduction
Paguroidea is a parser generator (a.k.a. the compiler of compiler). The theoretical foundation of Paguroidea is built
on a few papers:

- [Regular-expression derivatives reexamined](https://www.ccs.neu.edu/home/turon/re-deriv.pdf) introduced a way to generate
  DFAs for lexer directly based on language derivatives. The number of states in DFAs created by this approach is close to
  the minimal.
- [A Typed, Algebraic Approach to Parsing](https://www.cl.cam.ac.uk/~nk480/parsing.pdf) provides a method to "type check"
  context-free grammars such that the checked grammar is guaranteed to be parsed in linear time with single-token
  lookahead. This is especially useful in Flap/Paguroidea to make sure the total correctness of normalization.
- [flap: A Deterministic Parser with Fused Lexing](https://arxiv.org/abs/2304.05276v2) invented a novel approach to normalize
  context-free grammars into the so-called Deterministic Greibach Normal Form (DGNF), where one can use localized smaller lexers
  for each individual parser routine rather than lexing the whole input with a lexer containing all regular expressions of
  all tokens.

We modified the work of flap by extending DGNF with tree-generation actions, which are similar to the "reduce" operation in a
traditional shift-reduce parser.

## How to use
> **Notice**: grammar for parser definitions used in this section will be changed in the near future.

It is simple: just define your grammar and pass it to our compiler. Then, Paguroidea will output a standalone parser file.

For example, a simple S-expression parser can be defined as the following
```text
lexer {
    definition BLANK     = ' ';
    definition DIGIT     = '0' .. '9';
    definition ALPHA     = 'a' .. 'z' | 'A' .. 'Z';
    active token LPAREN     = '(';
    active token RPAREN     = ')';
    active token ATOM       = ALPHA ~ (ALPHA | DIGIT)*;
    silent token WHITESPACE = (BLANK | '\t' | '\n' | '\r')+;
}
parser sexpr {
    active fixpoint compound
        = LPAREN ~ (compound | atom) * ~ RPAREN;

    active definition atom
        = ATOM;

    active definition sexpr
        = compound | atom;
}
```

<details>
<summary>How to write a grammar file</summary>

You can put up your own one with the following rules:

- A grammar file must contain both lexer and parser parts.
- A `definition` in lexer part is a `macro` representing some common lexical rules. A definition
  itself does not count as a token, which is similar to `fragment` in ANTLR.
- A lexer can atmost have one `silent` token. `silent` tokens will be automatically skipped during
  parsing.
- All rules defined in lexer part must be full uppercase.
- You can use
  - empty (`'_'`)
  - characters (`'a', '\x12', '😊'`)
  - strings (`"你好", "Rust"`)
  - ranges (`'A' .. 'Z'`)
  - sequences (`'a' ~ 'b'`),
  - alternatives (`'a' | 'b'`)
  - optionals (`'a'?`)
  - zero-or-mores (`'a'\*`)
  - one-or-mores (`'a'+`)
  - complements (`!'a'`)

  to make up your regular expressions. Notice that complement is not negative lookahead in the common sense. Rather,
  it represents characters or languages complement to negated one. It is required that all active tokens cannot be nullable.
- The parser part must have an entrypoint specified in the header.
- Strings/characters/ranges cannot be directly used in the parser part, but parser can refer to tokens defined in lexer.
- Parser rules are all in lowercase.
- Most combinators in the lexer part are also supported in the parser part except for complement.

For more complicated examples, one can see [json.pag](benches/json/json.pag).
</details>

<details>
<summary>How to compile and use a grammar file</summary>

To compile your grammar file, the recommended way is to add `pag-compiler` as your build dependency. With `pag-compiler`,
the parser file can be easily generated in a build script as the following:
```rust
fn main() {
    pag_compiler::compile("csv.pag", "src/parser.rs");
    println!("cargo:rerun-if-changed=csv.pag");
}
```

For some reasons (mostly performance issues), only nightly rust (1.71+) is supported for now. It is also required that the crate containing the parser file
should be annotated with
```rust
#![feature(portable_simd)]
#![feature(core_intrinsics)]
#![feature(array_chunks)]
```
</details>

## Performance

We are continuously working on improvement the quality of our generated parser. For now, on workloads of CSV/JSON,
the performance is close to or even better than those specialized parsers.
```
=== Random Generated CSV ===
throughput/pag          time:   [635.88 µs 637.64 µs 639.46 µs]
                        thrpt:  [622.63 MiB/s 624.41 MiB/s 626.14 MiB/s]
throughput/csv          time:   [528.36 µs 541.72 µs 559.54 µs]
                        thrpt:  [711.56 MiB/s 734.97 MiB/s 753.55 MiB/s]
throughput/pest         time:   [3.7278 ms 3.7364 ms 3.7460 ms]
                        thrpt:  [106.29 MiB/s 106.56 MiB/s 106.80 MiB/s]
=== Random Generated JSON ===
random-json/pag-json    time:   [22.634 ns 22.650 ns 22.666 ns]
                        thrpt:  [84.149 MiB/s 84.209 MiB/s 84.271 MiB/s]
random-json/serde-json  time:   [12.493 ns 12.587 ns 12.694 ns]
                        thrpt:  [150.26 MiB/s 151.54 MiB/s 152.67 MiB/s]
random-json/pest-json   time:   [177.38 ns 178.17 ns 179.17 ns]
                        thrpt:  [10.645 MiB/s 10.705 MiB/s 10.753 MiB/s]
=== twitter.json ===
twitter-json/pag-json   time:   [1.0923 ms 1.0941 ms 1.0961 ms]
                        thrpt:  [667.24 MiB/s 668.46 MiB/s 669.59 MiB/s]
twitter-json/serde-json time:   [1.2281 ms 1.2295 ms 1.2312 ms]
                        thrpt:  [594.02 MiB/s 594.88 MiB/s 595.54 MiB/s]
twitter-json/pest-json  time:   [5.2977 ms 5.3055 ms 5.3148 ms]
                        thrpt:  [137.61 MiB/s 137.85 MiB/s 138.06 MiB/s]
```

<details>
<summary>Why is it fast and how can I make my grammar faster</summary>

- Thanks to the work of the Flap parser, we can fuse lexer and parser together such that lexers can be localized.
- We apply tail-call optimizations explicitly. To utilize this feature, default more grammar rules using `*`, `+` or
  mark them rule as silent rules if possible.
- We apply batched lookahead strategy using SIMD or lookup tables. This optimization applies when you repeat simple character sets
  (for instance, `(BLANK | '\t' | '\n' | '\r')+`).
- We are working on to inline/reduce more operations involving state transitions and lexer-parse communications.
</details>

## Diagnostic Grammar Error Check
We provide diagnostic information for "type errors" in your grammar definitions. Here are some examples:

**Left-recursion**
```
  Error: Unguarded fixpoint
      ╭─[json.pag:39:5]
      │
   39 │     active fixpoint json = json ~ value;
      │     ─────────────────┬─────────────────
      │                      ╰─────────────────── fixpoint rule json is not guarded -- your grammar is left-recursive
  ────╯
```
**Sequence Ambiguity**

> **Explanation**: there may be ambiguity when separating a sequence into two part according to the grammar definition

```
  Error: When type checking a sequence of rules, the following rules are ambiguous
      ╭─[json.pag:39:28]
      │
   39 │     active fixpoint test = NUMBER+ ~ NUMBER+;
      │                            ───┬───   ───┬───
      │                               ╰─────────────── type info for left-hand side: nullable: false, first set: {NUMBER}, follow set: {NUMBER}
      │                                         │
      │                                         ╰───── type info for right-hand side: nullable: false, first set: {NUMBER}, follow set: {NUMBER}
  ────╯
```

**Alternation Ambiguity**
> **Explanation**: there may be ambiguity when select a match in an alternation of two rules.
```
  Error: When type checking an alternation of rules, the following rules are ambiguous
      ╭─[json.pag:39:28]
      │
   39 │     active fixpoint test = NUMBER+ | NUMBER;
      │                            ───┬───   ───┬──
      │                               ╰────────────── type info for left-hand side: nullable false, first set: NUMBER, follow set: NUMBER
      │                                         │
      │                                         ╰──── type info for right-hand side: nullable false, first set: NUMBER, follow set:
  ────╯
```

There are other diagnostic information for undefined references, nullable tokens in lexer, character format error, etc.
