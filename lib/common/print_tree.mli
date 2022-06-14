(*** Pretty-print a tree. 
 *
 * childs_below: indicates if the first child is put below the parent node, instead of on the same line.
 *               None : same line
 *               Some n : below the parent none with n indentation
 **)

val sprint_tree: ?childs_below:('a -> int option) -> ('a -> string) -> ('a -> 'a list) -> 'a -> string


