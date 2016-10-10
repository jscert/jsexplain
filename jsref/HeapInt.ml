
let empty =
  Heap.empty

let read l k = 
  Heap.read int_compare l k

let write l k v =
  Heap.write int_compare l k v

let rem l k =
  Heap.rem int_compare l k 

let read_option l k =
  Heap.read_option int_compare l k 

let indom_dec l k = 
  Heap.indom_dec int_compare l k

let to_list l =
  Heap.to_list int_compare l

