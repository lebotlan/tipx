#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/memory.h"

// external pick_loop : int -> Bytes.t -> int -> int -> int = "c_pick_loop"

// bv is an array of length len.  len must be a multiple of 8.
int c_pick_loop_aux(int len, char* bv, int start_index, int index) {

  /*
  Il y a des vieilles astuces pour être salement efficace.

 find_next_bit

 https://code.woboq.org/linux/linux/lib/find_bit.c.html

  */
  
}



CAMLprim value c_pick_loop (value len, value bv, value start_index, value index)
{
  CAMLparam4 (len, bv, start_index, index) ;
  CAMLreturn (Val_int( c_pick_loop_aux(Int_val(len), Bytes_val(bv), Int_val(start_index), Int_val(index)) )) ;
}

