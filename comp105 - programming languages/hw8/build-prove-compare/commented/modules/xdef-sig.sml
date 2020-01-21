(* <xdef.sig>=                                  *)
signature XDEF = sig
  type def
  type name = string
  type unit_test
  (* <definition of [[xdef]] (shared)>=           *)
  datatype xdef = DEF    of def
                | USE    of name
                | TEST   of unit_test
                | DEFS   of def list  (*OMIT*)
end
