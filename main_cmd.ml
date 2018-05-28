(** This file only exists to execute Terminal_UI's main function, starting the
 *  command line version of Stratego. the only reason this function invocation
 *  is not in terminal_UI.ml is because the corresponding package is opened by
 *  GUI.ml and it makes sense to have the code corresponding to the command line
 *  code in terminal_UI.ml **)
open Terminal_UI

let _ = main()
