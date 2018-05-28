open Board

(** [clear_screen()] simply clears the terminal's content while also not
  * allowing the player to scroll up to a previous display.
  **)
val clear_screen : unit -> unit

(**[directory_name] is a constant representing where all the .txt files used in
 * the terminal are stored **)
val directory_name : string

(**[get_string_from_file f_name] gets the string contents of the file whose
 * address relative to the byte code is [f_name]
 * Precondition: f_name is an address to an actual file. **)
val get_string_from_file :  string -> string

(* Signature for modules that interact with board via a terminal *)
module type Terminal = sig
  (** [play_2 ()] plays a 2-player game of the propper version of Stratego
    * according to the Board module passed to this functor **)
  val play_2 : unit -> unit

  (**[play_ai()] facilitates play between a player and an AI *)
  val play_ai : color -> unit -> unit
end

(* Modules to represent the command line interface accessed when the user wants
 * to play the 8-square version and the 10-square version of Stratego*)
module CL8 : Terminal

module CL10 : Terminal

(** [main ()] Begins execution of the terminal UI of Stratego **)
val main : unit -> unit
