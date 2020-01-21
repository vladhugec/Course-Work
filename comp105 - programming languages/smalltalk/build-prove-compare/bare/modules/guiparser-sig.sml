(* guiparser.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature GUIPARSER = sig
  type xdef
  type line = string
  type srloc
  
  (********* structure for parsing *********)

  type input  (* input awaiting parsing *)
  val emptyInput : input
  val addLines   : input * srcloc * line list -> input
                                                    (* append to end of input *)

  type code_block (* represents input that failed to parse *)
  type leftover = input  (* input that was not consumed *)
  datatype result = SYNERROR of string * code_block * leftover
                  | PARSED   of xdef * leftover
                  | INCOMPLETE
                  | EOF
  val parseOne : input -> result

  (********** Display for users? ************)

  val emptyInput : input -> bool
  val inputDiff  : input * input -> line list  

   (* e.g., case parse ipt
              of PARSED (xd, leftover) => showParsed (inputDiff (ipt, leftover))
                                                                              *)

end
