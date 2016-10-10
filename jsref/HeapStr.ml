
let empty =
  Heap.empty

let read l k = 
  Heap.read string_compare l k

let write l k v =
  Heap.write string_compare l k v

let rem l k =
  Heap.rem string_compare l k 

let read_option l k =
  Heap.read_option string_compare l k 

let indom_dec l k = 
  Heap.indom_dec string_compare l k

let to_list l =
  Heap.to_list string_compare l

