val add : 'a -> 'a -> 'a
val ( + ) : 'a -> 'a -> 'a
val sub : 'a -> 'a -> 'a
val ( - ) : 'a -> 'a -> 'a
val mul : 'a -> 'a -> 'a
val ( * ) : 'a -> 'a -> 'a
val div : 'a -> 'a -> 'a
val ( / ) : 'a -> 'a -> 'a
                 
val eq : 'a -> 'a -> bool
val ( === ) : 'a -> 'a -> 'a
val le : 'a -> 'a -> bool
val ( < ) : 'a -> 'a -> 'a
val ge : 'a -> 'a -> bool
val ( > ) : 'a -> 'a -> 'a

val leq : 'a -> 'a -> bool
val ( <= ) : 'a -> 'a -> 'a
val geq : 'a -> 'a -> bool
val ( >= ) : 'a -> 'a -> 'a

val print : 'a -> unit

val to_string : 'a -> string
val parse : 'a -> 'b
