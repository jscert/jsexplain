type number = float

val floor : float -> float


val zero: number
val neg_zero : number
val one : number
val infinity : number
val neg_infinity : number
val max_value : number
val min_value : number
val nan : number
val pi : number
val e : number
val ln2 : number

val from_string : string -> number
val to_string : number -> string

val to_int32 : number -> number
val to_uint32 : number -> number

val int32_left_shift : number -> number -> number
val int32_right_shift : number -> number -> number

val int32_bitwise_and : number -> number -> number
val int32_bitwise_or : number -> number -> number
val int32_bitwise_xor : number -> number -> number
val int32_bitwise_not : number -> number

val uint32_right_shift : number -> number -> number

val neg : number -> number
val sign : number -> number
val absolute : number -> number

val modulo_32 : number -> number
