# ppx\_match\_literals
---

**ppx\_match\_literals** is an OCaml extension written using [ppxlib](https://github.com/ocaml-ppx/ppxlib) 
that allows for variables to be placed in `match` patterns and have them be interpreted as their 
literal value.

### How to use as literals

To specify that a `match` expression will have patterns that require this extension, 
create the `match` statement using the extension `%literals` with infix syntax; 
ie, use `match%literals` instead of `match` when going to use literal variables in the cases.
Inside a `match%literals` expression, literal variables can be specified in a case pattern using the `%lit` 
extension with the variable name as the parameter. Usage would look like `[%lit var_name_here]`.

### How it works

This extension traverses the patterns of all `match%literals` expressions and replaces the extension nodes 
representing `[%lit id]` patterns with wildcard patterns. An extra `when` guard is appended for every 
wildcard that needs to be matched to a variable. To avoid namespace collision, wildcards are given 
the name `ppx_match_literals_lit_*` where "*" is replace with the name of the variable to match.

### Example usage

*Before desugaring, using extension*
```
let hello = "hello" in
let hi = "hi" in
match%literals ("hello", "abc", "hi") with
| ([%lit hello], "abc", _) when boolean_1 -> 1
| (_, "123", [%lit hi]) -> 2
| ([%lit hello], "abc", [%lit hi]) -> 3
| _ -> 4
```

*After desugaring*
```
let hello = "hello" in
let hi = "hi" in
match ("hello", "abc", "hi") with
| (ppx_match_literals_lit_hello, "abc", _) when 
    boolean_1 && ppx_match_literals_lit_hello = hello -> 1
| (_, "123", ppx_match_literals_lit_hi) when ppx_match_literals_lit_hi = hi -> 2
| (ppx_match_literals_lit_hello, "abc", ppx_match_literals_lit_hi) when 
    ppx_match_literals_lit_hello = hello && ppx_match_literals_lit_hi = hi-> 3
| _ -> 4
```
