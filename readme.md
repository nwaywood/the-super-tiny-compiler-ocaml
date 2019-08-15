# the-super-tiny-compiler-ocaml

Implementation of [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) in OCaml.

## Comments

Writing this compiler required writing 4 seperate pieces: the lexer, the parser, the transformer and the generator.

The lexer and parser just required coverting to a recursive solution from an imperative one, which some pattern matching tricks required for the parser (Modifying a type contructor after it was created by destructuring it again with pattern matching)


## Related links

- https://medium.com/@javierwchavarri/building-the-super-tiny-compiler-with-reason-part-1-21460cd4ae7b


