

let empty =
  Heap.empty

let read l k = 
  Heap.read JsSyntaxAux.object_loc_cmp l k

let write l k v =
  Heap.write JsSyntaxAux.object_loc_cmp l k v

let rem l k =
  Heap.rem JsSyntaxAux.object_loc_cmp l k 

let read_option l k =
  Heap.read_option JsSyntaxAux.object_loc_cmp l k 

let indom_dec l k = 
  Heap.indom_dec JsSyntaxAux.object_loc_cmp l k

let to_list l =
  Heap.to_list JsSyntaxAux.object_loc_cmp l

