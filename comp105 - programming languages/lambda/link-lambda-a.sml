structure Go = InterpreterFun(structure Solution = struct
                                open SealedSolution (* experts only *)
                                val reduceN = SealedSolution.reduceA
                              end
                              val run = true
                              val showStep = false
                             )
