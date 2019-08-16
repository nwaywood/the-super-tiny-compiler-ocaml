# the-super-tiny-compiler-ocaml

Implementation of [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) in OCaml.

## Comments

During the process of migrating the JavaScript to OCaml, there were a few things that I thought were noteworthy.

Lexer: The lexer just required converting to a recursive solution from an imperative one, nothing out of the ordinary here.

Parser: The parser also required implementing a recursive solution, but in addition required some fancy pattern matching. It required modifying type contructors after they were created by destructuring them again with pattern matching, this is something I have never done before and thought it was an interesting.

Transformer: The transformer in the JavaScript code used the visitor design pattern for defining how to handle the traversed nodes. This becomes much simpler in OCaml due to the power of pattern matching.

Generator: The generator is basically a direct port.

## Related links

- https://medium.com/@javierwchavarri/building-the-super-tiny-compiler-with-reason-part-1-21460cd4ae7b


