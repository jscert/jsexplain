type bool =
| Coq_true [@f]  (** Auto Generated Attributes **)
| Coq_false [@f]  (** Auto Generated Attributes **)

type nat =
| O [@f]  (** Auto Generated Attributes **)
| S  [@f label0] of nat (** Auto Generated Attributes **)

type 'a list =
| Coq_nil [@f]  (** Auto Generated Attributes **)
| Coq_cons  [@f label0, label1] of 'a * 'a list (** Auto Generated Attributes **)

