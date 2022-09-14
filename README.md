# Projet Traduction des Langages

<a href="https://ocaml.org/">
  <img src="https://img.shields.io/badge/language-OCaml-f18d00?style=flat-square" alt="laguage-eCore" />
</a>

---

Development of a compiler for the RAT language

A pure functional programming style is required. The only side effects allowed are those on the information associated with an identifier.
As a reminder, the evaluation order of OCaml is not intuitive (last parameter of a tuple first, last parameter of a function first, right operand then left operand, etc.). This has no impact in pure functional programming, but it does when there are side effects. If there are edge effects in `xxx` and `yyy`, write something like :

```OCaml
let e1 = xxx in
  let e2 = yyy in
    (e1,e2)
```

where you choose the order of evaluation (order of let ... in), rather than `(xxx,yyy)` which lets OCaml choose the order of evaluation (`yyy` will be evaluated first).
