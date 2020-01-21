(* interactivity.sig (THIS CAN'T HAPPEN -- claimed code was not used) *)
signature INTERACTIVITY = sig
  datatype input_interactivity = PROMPTING | NOT_PROMPTING
  datatype output_interactivity = PRINTING | NOT_PRINTING
  type interactivity = input_interactivity * output_interactivity
  val noninteractive : interactivity
  val prompts : interactivity -> bool
  val prints  : interactivity -> bool
end
