# Log and Generation Recap
### What stage is this at?
Right now, the generator is capable of creating nearly ready javascript on the calc example. The `run_trm` wrap has been removed from this branch but a single `make tests` in `js_of_ocaml-advlog` generates navig.html ready code that just needs to be included.

### What's missing?
In the immediate, the if then else pattern in generated javascript doesn't have according `return` call logging. This can be tested in the `tests/mylist.ml` example.
The generation and updating of ctx inside of a function is also incorrect (in some instances): It would be best to have differently named contexts, ie:

```ocaml
> [A] - representation of the variables in context
let A':=
	match A return attributes with
    | attributes_data_of Ad =>
>    [A, Ad]
        attributes_accessor_of_attributes_data Ad
    | attributes_accessor_of Aa =>
>    [A, Aa]
        attributes_data_of_attributes_accessor Aa
    end 
in
>[A, A']
...
```
In this example, we would be accidentally adding Ad and Aa to the context outside of the `let` because of our use of a single ctx name. Multi names can be implemented at any level, the easiest being `js_of_ast` where we still have information as to what context we are stepping into and out of, then sending a generated ctx name to the logger to use.

### Adding new logging information
`js_of_ast` notifies the logger where it wants a "log context" via a call to `L.log_line` which takes a string and a list of `ctx_operation`.
These operations are defined twice inside log.ml. Both need to be identical. First occurence is line 34.

These contexts will then get recorded and signalled in a generated line via a token (form: `|<number>|`).
`log_line` searches for a new line or break, which may not exist yet in a string in the OCaml Print Format.


The context must then be matched inside the `add_line` `aux` recursive function, which steps through each token list/line pair and completes the action the token is defined to do inside the following match. The catchall skips any non matched tokens.

The final string is reconstructed via a Buffer, so add your logging lines in the same manner, ie:
```
    | Exit ->
        (* Log exit of a function *)
        Buffer.add_string buf ("\n" ^ pad ^ "log_custom({line:" ^ string_of_int (i + 1) ^ ", type: \"exit\"});");
        aux i ((tks, str) :: xs)
```
It doesn't matter if the context doesn't also add the original string back in the buffer, as long as you call `aux` recursively with the same i and the remaining tokens. You can also modify the original string by sending the modified version instead of the `str` seen in the example above.