type bool =
| Coq_true
| Coq_false

type nat =
| O
| S of nat

type 'a list =
| Coq_nil
| Coq_cons of 'a * 'a list

