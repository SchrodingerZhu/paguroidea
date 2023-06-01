<!--
 
-->
<h1 align="center">
<!--suppress CheckImageSize -->
<img src=".github/images/hermit-crab.png" alt="hermit-crab" width="50%" height="50%">
<br>
Paguroidea
</h1>
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
