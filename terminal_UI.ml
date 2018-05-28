(**
 * CREDITS:
 * All ASCII generated was made useing the following website:
 * http://patorjk.com/software/taag/#p=display&f=Graffiti&t=Type%20Something%20
 *
 * The Unix command used in the system call in clear_screen() was obtained
 * from the following page:
 * https://stackoverflow.com/questions/2198377
 **)

open Board
open Ai

let clear_screen () = ignore(Sys.command "clear && printf '\\e[3J'")

let directory_name = "terminal_text"^Filename.dir_sep

(* [get_string_from_file f_name] gets the string contents of the file whose
 * address relative to the byte code is [f_name]
 * Precondition: f_name is an address to an actual file. *)
let get_string_from_file f_name =
  let rec str_help chan ac =
    match input_line chan with
    | exception End_of_file -> ac^"\n"
    | line -> str_help chan (ac^line^"\n")
  in
  let input = open_in f_name in
  let output_string = str_help input "" in
  close_in input;(); output_string

(**
 * [print_file f_name c] displays the contents of the file whose address
 * relative to the byte code is [f_name] in the ANSITerminal styple c
 * Precondition: f_name is an address to an actual file.
 *)
let print_file f_name c =
  let rec print_help chan ac =
    match input_line chan with
    | exception End_of_file -> print_newline()
    | line -> ANSITerminal.(print_string [ac] (line^"\n"); print_help chan c)
  in
  let input = open_in f_name in
  print_help input c; close_in input;()

(* [ask q ops] asks the user to choose an option useing [q] and matches
 * the user's input with the first element of [ops] elements. If there is a
 * match, the corresponding second parameter of [ops] is returned. else, an
 * the user is prompted to type a new answer.*)
let rec ask q ops =
  let () = print_endline q; print_newline (); in
  let dec = read_line() |> String.trim |> String.uppercase_ascii in
  match (List.find_opt (fun (s,_) -> s = dec) ops) with
  | Some (_,f) -> f
  | None ->
    let () = print_endline
        ("ERROR: You did not type an acceptable option.\n" ^
                            "Here they are again: ") in
    let _ = List.map (fun (s,_) -> print_endline("\t*  "^s)) ops in
    print_newline (); ask q ops


(**
 * [str_to_rank s] takes the name of the rank ([s]) and outputs the actual
 * rank.
 * Requires [s] to be in all lowercase.
 *)
let str_to_rank s =
  match s with
  | "flag" -> Flag
  | "bomb" -> Bomb
  | "spy" -> Spy
  | "scout" -> Scout
  | "miner" ->Miner
  | "sergeant" -> Sergeant
  | "lieutenant" -> Lieutenant
  | "captain" -> Captain
  | "major" -> Major
  | "colonel" -> Colonel
  | "general" -> General
  | "marshal" -> Marshal

  | _-> failwith "Unkown Rank"

 (** [str_is_rank s] returns true if the string corresponds to a rank and
  * false otherwise.
  * Precondition: [s] is all lowercase
  *)
let str_is_rank s =
    try
      let _ = str_to_rank s in true
    with
    | Failure _-> false

(* [rank_to_string r] returns the string corresponding to a rank [r] *)
let rank_to_string = function
  | Flag -> "Flag"
  | Bomb -> "Bomb"
  | Spy -> "Spy"
  | Scout -> "Scout"
  | Miner -> "Miner"
  | Sergeant -> "Sergeant"
  | Lieutenant -> "Lieutenant"
  | Captain ->  "Captain"
  | Major -> "Major"
  | Colonel -> "Colonel"
  | General -> "General"
  | Marshal -> "Marshal"


(* [rank_to_char r] returns the character corresponding to a rank [r] *)
let rank_to_char = function
  | Flag -> 'F'
  | Bomb -> 'B'
  | Spy -> 'S'
  | Scout -> '2'
  | Miner -> '3'
  | Sergeant -> '4'
  | Lieutenant -> '5'
  | Captain -> '6'
  | Major -> '7'
  | Colonel -> '8'
  | General -> '9'
  | Marshal -> 'M'

(**
 * For use in last move info; shows both name and symbol of given rank
 * unless that piece's rank has not been revealed yet.
 * Returns "piece" for pieces that have not been revealed.
 *)
let name_and_symbol_if_visible p =
  if p.v = Revealed then
    (rank_to_string p.r) ^ " [" ^ Char.escaped (rank_to_char p.r) ^ "]"
  else "piece"

let string_of_col = function
  | 0 -> "A"
  | 1 -> "B"
  | 2 -> "C"
  | 3 -> "D"
  | 4 -> "E"
  | 5 -> "F"
  | 6 -> "G"
  | 7 -> "H"
  | 8 -> "I"
  | 9 -> "J"
  | _ -> failwith "Invalid Index"

module type Terminal = sig
  val play_2 : unit -> unit
  val play_ai : color -> unit -> unit
end

module type Com = sig

  (**
   * Represents the commands that a user may type in; see controls.txt
   * for all allowed commands and how to input them.
   *)
  type command =
    | Place of rank * space
    | Remove of space
    | Move of space * space
    | DisplayBoard
    | AutoSetup
    | ClearPreboard
    | Quit
    | FaultyCommand of string


  (**
   * [string_to_command s] returns a command corresponding to the string [s]
   * or a FaultyCommand constructor if there is something wrong with the
   * string [s].
   *)
  val string_to_command : string -> command

end

(**
 * This functor uses a Board module to create an interface with which the
 * user can type moves to make, display the current state of a Stratego
 * game, and execute those commands, either modifying the state or
 * providing the user with information. *)
module TerminalMaker (B:Board): Terminal = struct

  (* The AI that the user interface utilizes needs to work with the
   * same board size as B. *)
  module AI = MakeBoardAI(B)

  module Command = struct
    type command =
      | Place of rank * space
      | Remove of space
      | Move of space * space
      | DisplayBoard
      | AutoSetup
      | ClearPreboard
      | Quit
      | FaultyCommand of string

    (* returns [true] for lowercase letters only *)
    let is_letter c = Char.compare c 'a' >= 0 && Char.compare c 'z' <= 0

    (* returns true for digits 0 to 9 *)
    let is_number c = Char.compare c '0' >= 0 && Char.compare c '9' <= 0

    (**
     * [make_move s] returns the move command associated with the string
     * input [s] or fails if [s] is syntactically invalid.
     * Precondition: [s] is lowercase and trimmed.
     *)
    let make_move s =
      let s = (if String.length s >= 5 && String.sub s 0 5 = "move "
               then String.sub s 5 (String.length s - 5)
               else s) |> Str.global_replace (Str.regexp " ") "" in
      try
        if String.length s = 4 then
          let (r,c) = (int_of_string(String.sub s 0 1),
                    int_of_char(String.get s 1 ) - int_of_char 'a') in
          let (r1,c1) = (int_of_string(String.sub s 2 1),
                    int_of_char(String.get s 3 ) - int_of_char 'a') in
          Move ((r,c), (r1,c1))

        else if String.length s = 3 && String.get s 2 |> is_number then
          let (r,c) = (int_of_string(String.sub s 0 1),
                       int_of_char(String.get s 1 ) - int_of_char 'a') in
          let (r1,c1) = (int_of_char(String.get s 2), c) in
          Move ((r,c), (r1,c1))

        else if String.length s = 3 && String.get s 2 |> is_letter then
          let (r,c) = (int_of_string(String.sub s 0 1),
                       int_of_char(String.get s 1 ) - int_of_char 'a') in
          let (r1,c1) = (r, Char.code(String.get s 2) - int_of_char 'a') in
          Move ((r,c), (r1,c1))

        else failwith "Invalid Set of Locations"

        with
        | Failure _
        | Invalid_argument _ ->
          FaultyCommand "It appears that you typed an invalid move command."

    (**
     * [make_place s] returns the place command associated with the
     * string input [s] or fails if [s] is syntacticaly invalid.
     * Precondition: [s] is lowercase and trimmed.
     *)
    let make_place s =
      let s =
        if String.length s >= 6 && String.sub s 0 6 = "place "
        then String.sub s 6 (String.length s - 6)
        else s in
      try
        let rank_name = String.sub s 0 (String.index s ' ') in
        let rank = str_to_rank rank_name in
        let s = (String.sub s (String.length rank_name)
                   (String.length s - (String.length rank_name)))
                |> Str.global_replace (Str.regexp " ") "" in

        let (r,c) = (int_of_string(String.sub s 0 1),
                     int_of_char(String.get s 1 ) - int_of_char 'a') in
        Place(rank,(r,c))
      with
      | Invalid_argument _ -> FaultyCommand "Invalid Placement"
      | Failure _ -> FaultyCommand "Invalid Coordinates"
      | Not_found ->
          FaultyCommand "You typed the placement command incorrectly!"

    (**
     * [make_remove s] takes the user's input [s] and outputs a Remove
     * command. It raises an exception if the user's input does not match a
     * valid format for a remove command. Whether the command itself is
     * valid is decided in a different function.
     * Precondition: [s] is all lower case and trimmmed.
     *)
    let make_remove s =
      let s =
        if String.length s >= 7 && String.sub s 0 7 = "remove "
        then String.sub s 7 (String.length s - 7) |>
             Str.global_replace (Str.regexp " ") ""
        else failwith"Invalid remove command" in
      try
        let (r,c) = (int_of_string(String.sub s 0 1)),
                  int_of_char(String.get s 1 ) - int_of_char 'a' in
        Remove(r,c)
      with
      | Failure _ -> FaultyCommand "Nonexistant Location!"

    (**
     * [string_to_command s] takes a user input [s] and returns the proper
     * command.  It returns the FautlyCommand constructor instead if the
     * syntax of the user's input is invalid.
     *)
    let string_to_command s =
      let s = String.trim(String.lowercase_ascii s) in
      try
        if String.length s = 0 then
          FaultyCommand "Please type in a command."
        else if (String.length s >= 5 && String.sub s 0 5 = "move ") ||
         (String.get s 0 |> is_number)
        then make_move s
        else if (String.length s >= 6 && String.sub s 0 6 = "place " )||
                (String.contains s ' ' &&
                 String.sub s 0 (String.index s ' ') |> str_is_rank)
        then make_place s
        else if String.length s >= 7 && String.sub s 0 7 = "remove "
        then make_remove s
        else if s = "display" then DisplayBoard
        else if s = "auto" then AutoSetup
        else if s = "clear" then ClearPreboard
        else if s = "quit" then Quit
        else FaultyCommand "The command cannot be processed."
      with
      | Failure s -> FaultyCommand s

  end

  (**[print_row on_row color row] takes a list of pieces on a particular
   * row paired with the column it is on ([on_row]) and uses this to print
   * out the desired row as revealed from the [color] player's view.
   * The [row] number is also passed to make sure that a space isn't part
   * of a lake.
   * Precondition: [color] is the color of the current player.
   *)
  let print_row on_row color row =
    let () =
      match color with
      | Blue -> ANSITerminal.(print_string [blue] ((string_of_int row) ^ " "))
      | Red -> ANSITerminal.(print_string [red] ((string_of_int row) ^ " ")) in

    let column = ref 0 in
    while !column < B.size do
      (let piece_opt = List.find_opt (fun (c,p) -> !column = c) on_row in
      match piece_opt with
      | Some (_,piece)->(
        let dc = if piece.c=Red then ANSITerminal.red else ANSITerminal.blue in
        let rank_rep = Char.escaped(rank_to_char piece.r) in
        begin match piece.v with
          | Moved when piece.c <> color ->
            ANSITerminal.(print_string [dc] "(?) ")
          | Hidden when piece.c <> color ->
            ANSITerminal.(print_string [dc] "[?] ")
          | Revealed when piece.c <> color ->
            ANSITerminal.(print_string [dc] ("["^rank_rep^"] "))
          | Revealed ->
            ANSITerminal.(print_string [dc] ("!"^rank_rep^"! "))
          | Moved ->
            ANSITerminal.(print_string [dc] ("("^rank_rep^") "))
          | Hidden ->
            ANSITerminal.(print_string [dc] ("["^rank_rep^"] "))
        end)
      | None ->
        if List.mem (row, !column) B.lakes
        then ANSITerminal.(print_string [cyan]"[^] ")
        else ANSITerminal.(print_string [green] "[ ] "));
      column := !column + 1
    done

  (**
   * [db_helper act color row stop f] is a helper function for
   * displaying boards and preboards and prints the board as viewed by
   * player [color] so that his side of the board is at the bottom.
   *)
  let db_helper act color row stop f =
    let _ =
      while !row <> stop do
        let prepend acc ((r,c),p) = if !row = r then (c,p)::acc else acc in
        let on_row = List.fold_left prepend [] act in
        print_newline (); print_row on_row color !row; print_newline ();
        row := f !row
      done in
    let col = ref 0 in
    let dc = if color = Red then ANSITerminal.red else ANSITerminal.blue in
    let () = print_string "  " in
    while !col < B.size do
      let letter = Char.chr(!col + (Char.code 'A')) in
      ANSITerminal.(print_string [dc] (" "^(Char.escaped letter)^ "  "));
      incr col
    done;
    print_newline()

  (* displays the pieces captured so far *)
  let rec display_captured_pieces b =
    let () = print_endline
        "\t   *********\n\t   CAPTURED\n\t   PIECES\n\t   *********" in
    let () =  ANSITerminal.(print_string [red] ("RED\t\t\t");
                            print_string [blue] ("BLUE\n")) in
    let rec display_captured_pieces_helpper red blu std =
      let taken ra lst =
        match List.assoc_opt ra lst with
        | None -> 0
        | Some left -> left in
      match std with
      | [] -> ()
      | (ra,total)::t ->
        let r_num = taken ra red in
        let stat_red = rank_to_string ra^": "^string_of_int r_num
                       ^"/" ^string_of_int total in
        let b_num = taken ra blu in
        let stat_blue = rank_to_string ra^": "^string_of_int b_num
                        ^"/"^string_of_int total in
        ANSITerminal.(print_string [red] (stat_red^"\t\t"));
        ANSITerminal.(print_string [blue] (stat_blue^"\n"));
        display_captured_pieces_helpper red blu t
    in
    display_captured_pieces_helpper (B.captured_pieces  b Red )
    (B.captured_pieces b Blue) B.starting_pieces

  (**
   * [display_board b c] takes a board [b] and prints an ASCII
   * representation of the board onto the screen from the viewpoint of the
   * player corresponding to color [c].
   * Precondition: assumes all pieces visible to the opponent have a view
   * status of Revealed.
   *)
  let display_board b c =
    let top = B.size - 1 in
    let actives = B.active_pieces b in
    let row = ref (match c with | Red -> top | Blue -> 0) in
    (match c with
    | Red -> db_helper actives c row (-1) (fun x -> x - 1)
    | Blue -> db_helper actives c row B.size ((+)1));
    display_captured_pieces b

  (**
   *[display_unplaced c p] prints a list of unplaced pieces of player with
   * color [c] as well as their count.
   *)
  let display_unplaced c p =
    let pigment = if c = Red then ANSITerminal.red else ANSITerminal.blue in
    let show (r,i) =
      if i <> 0 then
        ANSITerminal.(
          print_string [pigment] ((rank_to_string r)^": "^string_of_int i^"\n")
        )
      else () in
    let print_sep () = ANSITerminal.(print_string [pigment] "----------\n") in
    let _ = print_sep (); List.map show (B.pieces_left p c) in
    print_sep()

  (**
   * [display_board p c] takes a preboard [p] and prints an ASCII
   * representation of the side of the board of the player corresponding
   * to color [c].
   *)
  let display_preboard c p =
    let top = B.size - 1 in
    let placed = List.map
        (fun (s,r) -> (s,{c=c; r=r; v=Hidden})) (B.placed_pieces p c) in
    let row =
      ref (match c with
          | Red -> top - B.starting_rows
          | Blue -> B.starting_rows) in
    begin match c with
      | Red -> db_helper placed c row (-1) (fun x -> x-1)
      | Blue -> db_helper placed c row B.size ((+)1)
    end; display_unplaced c p

  (**
   * [any_remaining c pb] returns true if not all the pieces of color [c]
   * have been placed onto preboard [pb].
   *)
  let pieces_remaining c pb =
    List.fold_left
      (fun acc (_,i) -> if i < 0 then failwith "Cannot have negative pieces"
        else acc + i) 0 (B.pieces_left  pb c)

  (**
   * [human_setup c p] is the preboard [p] after the human player whose
   * color is [c] has placed his/her pieces.
   *)
  let rec human_setup c pb =
    let remaining = pieces_remaining c pb in
    if remaining = 0 then
      let () = display_preboard c pb in
      let resp = ask "Is this what you would like? type Y for yes or N for no"
          [("Y","Y");("N","N")] in
      if resp = "Y" then pb else human_setup c (B.clear pb c)
    else
      let () = clear_screen();
        if c = Red then
          ANSITerminal.(print_string [red]
                          "*********\nRED'S TURN\n*********\n\n")
        else
          ANSITerminal.(print_string [blue]
                          "**********\nBLUE'S TURN\n**********\n\n");
        print_endline "Place your pieces onto the Board:\n";
        display_preboard c pb in
      let m = read_line() in
      Command.(  match string_to_command m with
          | Quit -> raise Exit
          | DisplayBoard -> (display_preboard c pb; human_setup c pb)
          | Place (r,s) ->
            if B.can_place_piece pb c r s then
              ANSITerminal.(
                if remaining <> 1 then print_string [yellow]
                    ("Successfully placed "^rank_to_string r^" on board\n\n" )
                else ();
                human_setup c (B.place_piece pb c r s))
            else (ANSITerminal.(print_string [magenta]
                            "ERROR:\nIllegal Placement\n\n"); human_setup c pb)
          | Remove s ->
            let new_board = B.remove_piece pb c s in
            let () =
              if List.length(B.placed_pieces pb c) =
                 List.length(B.placed_pieces new_board c) + 1 then
                ANSITerminal.(print_string [yellow]
                                "Piece successfully removed\n")
              else ANSITerminal.(print_string [magenta]
                                   "Illegal remove, command declined\n") in
            human_setup c new_board
          | AutoSetup -> AI.auto_setup c pb
          | ClearPreboard -> human_setup c (B.clear pb c)
          | FaultyCommand s ->
            ANSITerminal.(print_string [magenta] ("ERROR:\n"^s^"\n"));
            human_setup c pb
          | _ ->
            ANSITerminal.(print_string [magenta] (
                "ERROR:\nYou can only place or remove a piece " ^
                "during the setup phase\n"));
            human_setup c pb  )

  (**
   * [set_up_phase r_set b_set] plays the stage of Stratego where players
   * set their pieces onto the board. [r_set] and [b_set] are the functions
   * used to set up the red and blue player's side of the board
   * respectively. This is used to distinguish between human players and
   * the AI.
   *)
  let setup_phase r_set b_set =
    let pb = r_set B.empty in
    b_set pb

  (**
   * [is_present c b s] is true if there is a piece in space [s] whose
   * color is [c].
   *)
  let is_present c b s =
    List.exists (fun (s1,p) -> s1 = s && p.c = c) (B.active_pieces b)

  (**
   * Converts the information about the last move that the board provides
   * into a string that the command line can print.
   * This should not be printed on the AI's turn.
   * If no moves have happened yet, then this returns the empty string.
   *)
  let last_move_string b =
    (* current player is not the player who made the move *)
    let opp = B.current_player b in
    if B.turn_count b = 0 && opp = Red then ""
    else let i = B.last_move b in
      "The " ^ begin match opp with
        | Red -> "blue"
        | Blue -> "red"
      end ^ " " ^ name_and_symbol_if_visible i.moved ^
      " at " ^ (string_of_int (fst i.start_space)) ^
      (string_of_col (snd i.start_space)) ^ " moved to " ^
      (string_of_int (fst i.end_space)) ^
      (string_of_col (snd i.end_space)) ^
      match (i.defender,i.outcome) with
      | (None,None) -> "."
      | (Some d,Some o) -> ".\nIt " ^ begin match o with
          | Win -> "captured"
          | Loss -> "was captured by"
          | Tie -> "both captured and was captured by"
        end ^ " the " ^ begin match opp with
          | Red -> "red"
          | Blue -> "blue"
        end ^ " " ^ (name_and_symbol_if_visible d) ^ " there."
      | _ -> failwith "Impossible Last Move Info"

  (**
   * [human_play b] asks the user for a command after printing the current
   * state of the game (from the current player's view) along with the
   * proper messages.  The command is then processed, which may either
   * display the board, move a piece, quit the game, or ask the user to
   * type something else if the command does not fit the current phase of
   * the game or if the input does not match any pattern for a command.
   *)
  let rec human_play b =
    let c = B.current_player b in
    (* moved these print statements from choose_mover *)
    begin match c with
      | Red -> ANSITerminal.(print_string [red]
                               "-------\nRed Player's Turn\n-------\n\n")
      | Blue -> ANSITerminal.(print_string [blue]
                                "-------\nBlue Player's Turn\n-------\n\n")
    end;
    let () =
      display_board b c;
      print_endline(last_move_string b);
      print_string "\nEnter a command: " in
    Command.(match string_to_command(read_line()) with
        | DisplayBoard -> human_play b
        | Move(start,term) ->
          if is_present c b start && B.is_valid_move b start term
          then
            let new_board = B.move b start term in
            let () =
              ANSITerminal.(
                print_string [yellow ]((last_move_string new_board)^"\n");
                print_string [yellow] "Press Enter to continue. " ) in
            let _ = read_line() in
            new_board
          else
            ANSITerminal.(
              clear_screen();
              print_string [magenta] "ERROR: invalid move\n"; human_play b )
        | Quit -> raise Exit
        | AutoSetup | ClearPreboard | Place _ | Remove _ | FaultyCommand _ ->
          ANSITerminal.(
            clear_screen(); print_string [magenta]
              ("ERROR:\nOne can only move a piece or display the board in this"
               ^" phase of the game.\n");
            human_play b ) )

  (**
   * [ai_play b] takes in a B.board [b] and uses the AI's [choose_move]
   * function to decide what move the AI should make and then makes that
   * move to the board, returning the resulting board.
   *)
  let ai_play b = let (s1,s2) = AI.choose_move b in B.move b s1 s2

  (**
   * [print_victory c] takes a color [c] and prints the corresponding
   * victory message.
   *)
  let print_victory c =
    let () = clear_screen () in
    let () = match c with
      | Red -> ANSITerminal.(print_string [red]
            "***************\nRed Player Wins\n***************\n")
      | Blue -> ANSITerminal.(print_string [blue]
            "****************\nBlue Player Wins\n****************\n\n") in
    let () = print_endline "Press Enter when ready. " in
    let _ = read_line () in
    ()

  (**
   * [choose_mover b r_move b_move] decides which move function to execute
   * depending on the current player of [b].
   *)
  let choose_mover b r_move b_move =
    let () = clear_screen () in
    match B.current_player b with
    | Red -> r_move b
    | Blue -> b_move b

  (* display_buffer_screen b] takes in [b] to determine which player is
   * moving and displays the screen telling the opposite opponent to look
   * away. *)
  let display_buffer_screen b =
    let (name,c) =
      match B.current_player b with
      | Red ->("red_turn.txt", ANSITerminal.red)
      | Blue ->("blue_turn.txt", ANSITerminal.blue) in
    print_file (directory_name^name) c;
    print_string
      ("\n\n\nPress Enter when the opposite player has turned around. ");
    ignore(read_line ());
    clear_screen()

  (**
   * [play_game b r_move b_move dbs] plays out a game of Stratego with the
   * starting board [b]. displays buffer screens when need be if [dbs].
   *)
  let rec play_game b r_move b_move dbs =
    let _ = clear_screen() in
    match B.status_of_game b with
    | Timeout ->
      print_endline "*******\nTied Game\n*******\nPress Enter when Ready: ";
      ignore(read_line())
    | Finished c -> print_victory c
    | Ongoing ->
      let () = if dbs then display_buffer_screen b else () in
      let new_board = choose_mover b r_move b_move in
      play_game new_board r_move b_move dbs

  let play_2 () =
    let _ = ANSITerminal.resize 60 38 in
    try
      let b = setup_phase (human_setup Red) (human_setup Blue) in
      let () = if(B.size = 8) then () else ANSITerminal.resize 60 48 in
      play_game (B.start_game b) human_play human_play true
    with
    | Exit -> ()

  let play_ai human_color () =
    let _ = ANSITerminal.resize 60 38; clear_screen() in
    try
      match human_color with
      | Red ->
        let b = setup_phase (human_setup Red) (AI.auto_setup Blue) in
        let () = if(B.size = 8) then () else ANSITerminal.resize 60 48 in
        play_game (B.start_game b) human_play ai_play false
      | Blue ->
        let b = setup_phase (AI.auto_setup Red) (human_setup Blue) in
        let () = if(B.size = 8) then () else ANSITerminal.resize 60 48 in
        play_game (B.start_game b) ai_play human_play false
    with
    | Exit -> ()

end

module CL8 = TerminalMaker(Board8)

module CL10 = TerminalMaker(Board10)

(* [print_title ()] prints the name of the board game onto the screen. *)
let print_title () =
  print_file (directory_name^"title.txt") ANSITerminal.default ;
  print_newline ()

(**
 * [print_options lst] takes in a list of option*description pairs and
 * proceeds to print out each of them in a mennu-like manner.
 *)
let print_options lst =
  let _ = List.map (fun (c,e) -> print_endline("["^c^"]\t "^e)) lst in
  print_newline()

(**
 * [ai_pregame p10] asks the user what color he would want to be and begins
 * the proper 10 (if [p10]) or 8 board game.
 *)
let ai_pregame play_10 () =
  let ai_version = if play_10 then CL10.play_ai else CL8.play_ai in
  print_options[("R", "Red (First to Move)"); ("B", "Blue Second to Move")];
  ask "Select a game to play."[("R",ai_version Red ); ("B",ai_version Blue)]()

(* [terminate ()] ends execution of the terminal version of Stratego. *)
let terminate ()  = clear_screen(); ignore(exit 0)

let rec main () =
  let _ = ANSITerminal.resize 60 38; clear_screen() in
  print_title();
  print_options [("1", "Traditional Game (2P)"); ("2", "Quick Match (2P)");
                 ("3", "Traditional Game (1P)"); ("4", "Quick Match (1P)");
                 ("5", "Close Game")];
  let game = ask "Select a game to play."[("1",CL10.play_2); ("2",CL8.play_2);
                                          ("3",ai_pregame true );
                                          ("4",ai_pregame false);
                                          ("5",terminate)]in
  game(); clear_screen(); main()
