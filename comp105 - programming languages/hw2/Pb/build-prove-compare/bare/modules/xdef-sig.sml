(* xdef.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature XDEF = sig
  type def
  type name = string
  type unit_test
  (* definition of [[xdef]] (shared) 361c *)
  datatype xdef = DEF    of def
                | USE    of name
                | TEST   of unit_test
                | DEFS   of def list  (*OMIT*)
end
