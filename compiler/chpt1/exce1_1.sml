type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
    | insert (key, TREE(l, k, r)) = 
        if key < k 
                then TREE(insert(key, l), k, r)
            else if key > k 
                then TREE(l, k, insert(key, r))
            else TREE(l, key, r)

fun member (key, tree) = 
    case tree of 
        LEAF => false
        | TREE(l, value, r) => 
            if value = key 
                then false
            else if value > key 
                then member(key, r)
            else member(key, l)