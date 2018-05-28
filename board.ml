type space = int*int

type color = Red | Blue

type status = Ongoing | Timeout | Finished of color

type battle_outcome = Win | Loss | Tie

type rank =
  | Flag
  | Bomb
  | Spy
  | Scout
  | Miner
  | Sergeant
  | Lieutenant
  | Captain
  | Major
  | Colonel
  | General
  | Marshal

type visibility = Hidden | Moved | Revealed

type piece = {
  c: color;
  r: rank;
  v: visibility
}

type move_info = {
  start_space: space;
  end_space: space;
  moved: piece;
  defender: piece option;
  outcome: battle_outcome option
}

(**
 * This signature represents the constants associated with the board of
 * the corresponding size.  The number of row/column constructors
 * determines which board is being modeled.
 * RI:  The number of row cosntructors and the number of column
 * constructors are always equal.
 *)
module type Constants = sig

  type row
  type col

  val row_to_num : row -> int
  val col_to_num : col -> int

  (**
   * Precondition:
   * The given int is nonnegative and strictly less than the number of
   * constructors that exist for the corresponding variant.
   *)
  val row_from_num : int -> row
  val col_from_num : int -> col

  val size : int

  val lakes : (row * col) list

  val starting_pieces : (rank * int) list

  val timeout_limit : int

  val starting_rows : int

end


module Constants8 : Constants = struct

  type row = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  type col = CA | CB | CC | CD | CE | CF | CG | CH

  let row_to_num = function
    | R0 -> 0
    | R1 -> 1
    | R2 -> 2
    | R3 -> 3
    | R4 -> 4
    | R5 -> 5
    | R6 -> 6
    | R7 -> 7

  let col_to_num = function
    | CA -> 0
    | CB -> 1
    | CC -> 2
    | CD -> 3
    | CE -> 4
    | CF -> 5
    | CG -> 6
    | CH -> 7

  let row_from_num = function
    | 0 -> R0
    | 1 -> R1
    | 2 -> R2
    | 3 -> R3
    | 4 -> R4
    | 5 -> R5
    | 6 -> R6
    | 7 -> R7
    | _ -> raise (Failure "Out of Bounds")

  let col_from_num = function
    | 0 -> CA
    | 1 -> CB
    | 2 -> CC
    | 3 -> CD
    | 4 -> CE
    | 5 -> CF
    | 6 -> CG
    | 7 -> CH
    | _ -> raise (Failure "Out of Bounds")

  let size = 8

  let lakes = [(R3,CC);(R4,CC);(R3,CF);(R4,CF)]

  let starting_pieces = [
    (Flag,1);
    (Bomb,2);
    (Spy,1);
    (Scout,2);
    (Miner,2);
    (General,1);
    (Marshal,1)
  ]

  let timeout_limit = 50

  let starting_rows = 3

end

module Constants10 = struct

  type row = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
  type col = CA | CB | CC | CD | CE | CF | CG | CH | CI | CJ

  let row_to_num = function
    | R0 -> 0
    | R1 -> 1
    | R2 -> 2
    | R3 -> 3
    | R4 -> 4
    | R5 -> 5
    | R6 -> 6
    | R7 -> 7
    | R8 -> 8
    | R9 -> 9

  let col_to_num = function
    | CA -> 0
    | CB -> 1
    | CC -> 2
    | CD -> 3
    | CE -> 4
    | CF -> 5
    | CG -> 6
    | CH -> 7
    | CI -> 8
    | CJ -> 9

  let row_from_num = function
    | 0 -> R0
    | 1 -> R1
    | 2 -> R2
    | 3 -> R3
    | 4 -> R4
    | 5 -> R5
    | 6 -> R6
    | 7 -> R7
    | 8 -> R8
    | 9 -> R9
    | _ -> raise (Failure "Out of Bounds")

  let col_from_num = function
    | 0 -> CA
    | 1 -> CB
    | 2 -> CC
    | 3 -> CD
    | 4 -> CE
    | 5 -> CF
    | 6 -> CG
    | 7 -> CH
    | 8 -> CI
    | 9 -> CJ
    | _ -> raise (Failure "Out of Bounds")

  let size = 10

  let lakes = [(R4,CC);(R5,CC);(R4,CD);(R5,CD);
               (R4,CG);(R5,CG);(R4,CH);(R5,CH)]

  let starting_pieces = [
    (Flag,1);
    (Bomb,6);
    (Spy,1);
    (Scout,8);
    (Miner,5);
    (Sergeant,4);
    (Lieutenant,4);
    (Captain,4);
    (Major,3);
    (Colonel,2);
    (General,1);
    (Marshal,1)
  ]

  let timeout_limit = 50

  let starting_rows = 4

end

(**
 * This indicates which other ranks a piece of a given rank can capture.
 * Any mobile piece can capture a piece of its own rank, destroying
 * itself in the process, but a piece's own rank is not included in the
 * returned list.
 *)
let can_capture p =
  match p.r with
  | Spy -> [Marshal;Flag]
  | Scout -> [Spy;Flag]
  | Miner -> [Spy;Scout;Bomb;Flag]
  | Sergeant -> [Spy;Scout;Miner;Flag]
  | Lieutenant -> [Spy;Scout;Miner;Sergeant;Flag]
  | Captain -> [Spy;Scout;Miner;Sergeant;Lieutenant;Flag]
  | Major -> [Spy;Scout;Miner;Sergeant;Lieutenant;Captain;Flag]
  | Colonel -> [Spy;Scout;Miner;Sergeant;Lieutenant;Captain;Major;Flag]
  | General -> [Spy;Scout;Miner;Sergeant;Lieutenant;
                Captain;Major;Colonel;Flag]
  | Marshal -> [Spy;Scout;Miner;Sergeant;Lieutenant;
                Captain;Major;Colonel;General;Flag]
  | _ -> failwith "Immobile Piece"

(**
 * This indicates the outcome of a battle in which [att] is the attacker
 * and [def] is the defender.  This does not change any components of the
 * given pieces; it simply indicates the outcome.
 *)
let battle att def =
  if att.r = def.r then Tie
  else if List.mem def.r (can_capture att) then Win
  else Loss

(**
 * Removes all elements from a list except the first two, and preserves the
 * order of those two elements.  If the given list has no more than two
 * elements, this returns the same list.
 *)
let trim = function
  | [] -> []
  | h1::t -> begin match t with
      | [] -> h1::[]
      | h2::_ -> h1::h2::[]
    end

module type Board = sig
  type board
  type preboard
  val starting_rows : int
  val size : int
  val lakes : space list
  val starting_pieces : (rank * int) list
  val empty : preboard
  val placed_pieces : preboard -> color -> (space * rank) list
  val pieces_left : preboard -> color -> (rank * int) list
  val can_place_piece : preboard -> color -> rank -> space -> bool
  val place_piece : preboard -> color -> rank -> space -> preboard
  val remove_piece : preboard -> color -> space -> preboard
  val clear : preboard -> color -> preboard
  val start_game : preboard -> board
  val current_player : board -> color
  val turn_count : board -> int
  val status_of_game : board -> status
  val active_pieces : board -> (space * piece) list
  val captured_pieces : board -> color -> (rank * int) list
  val all_moves : board -> (space * space list) list
  val is_valid_move : board -> space -> space -> bool
  val move : board -> space -> space -> board
  val last_move : board -> move_info
end

module MakeBoard (C : Constants) : Board = struct

  (* These are some functions for type conversion and manipulation. *)
  let opposite = function
    | Red -> Blue
    | Blue -> Red

  (*[coord_to_ints (r,c)] converts the coordinate (r,c) to a space*)
  let coord_to_ints (r,c) =
    (C.row_to_num r,C.col_to_num c)

  (*[coord_to_ints (r,c)] converts the space (r,c) to a coordinate*)
  let ints_to_coord (r,c) =
    (C.row_from_num r,C.col_from_num c)

  (*[choose c r b] is [r] if [c] is Red and [b] is [c] is Blue*)
  let choose c r b =
    match c with
    | Red -> r
    | Blue -> b

  (* The internal representation of coordinates. *)
  type coord = C.row * C.col

  (**
   * Within the preboard, the statuses of red and blue pieces should be
   * stored separately.  This way, functions like [pieces_left] don't need
   * to filter through a list of pieces of both colors in order to find
   * the information they need.
   * Abstraction Function:
   * A (coord * rank) pair in a list of placed pieces corresponds to the
   * starting location of a piece of that rank when the game begins.
   * Representation Invariant:
   * No coord appears as a key more than once in the list
   * [placed_red @ placed_blue].
   * The ranks of all entries in [placed_red] and the ranks shown in
   * [unplaced_red], when added together, always equal the quantities
   * of ranks in [starting_pieces].  The same holds for [placed_blue]
   * and [unplaced_blue].
   *)
  type preboard = {
    placed_red: (coord * rank) list;
    placed_blue: (coord * rank) list;
    unplaced_red: (rank * int) list;
    unplaced_blue: (rank * int) list
  }

  (**
   * Abstraction Function:
   * The order of the entries in [active_pieces] is irrelevant.  The keys
   * of the entries in the list indicate pieces' locations.
   * The head entry of a history list represents the corresponding
   * player's most recent move, and the entry after that represents the
   * player's last move before that.  The key of an entry is the starting
   * location of the piece that moved, and the value is the end location.
   * The integer values in the graveyards indicate the number of pieces
   * of the corresponding rank and color that have been captured.
   * Representation Invariant:
   * No key appears more than once in [active_pieces] or either graveyard.
   * No lake tile location ever appears as a key in [active_pieces] or as
   * a key or value in either history list.
   * The ranks of all pieces of one color in [active_pieces] and the ranks
   * in the corresponding graveyard, when added together, always equal the
   * quantities of ranks in [starting_pieces].
   * The rank stored in the head entry of either graveyard is equal to the
   * rank of the piece of the corresponding color that was captured most
   * recently.  If no pieces of that color have been captured yet, the
   * graveyard is empty.
   * [turn_count] and [moves_without_battle] are always nonnegative.
   * [history_red] and [history_blue] are never more than two entries
   * long, and the only time when they are shorter than that is during the
   * first two turns of the game.
   *)
  type board = {
    active_pieces: (coord * piece) list;
    (* (John) I thought that making these two lists separate might
     * be helpful because it will allow us to use the association list
     * functions from the List library on them.  If no piece of a certain
     * rank has been captured yet, that rank should not appear in the
     * graveyard. *)
    graveyard_red: (rank * int) list;
    graveyard_blue: (rank * int) list;
    (* Each turn is two moves. Red moves first, then blue. *)
    turn_count: int;
    (* This is the number of MOVES, NOT TURNS, without a battle. *)
    moves_without_battle: int;
    history_red: (coord * coord) list;
    history_blue: (coord * coord) list;
    current_player: color
  }

  let starting_rows = C.starting_rows

  let size = C.size

  let lakes =
    List.map (fun (r,c) -> (C.row_to_num r, C.col_to_num c)) C.lakes

  let starting_pieces = C.starting_pieces

  let empty = {
    placed_red = [];
    placed_blue = [];
    unplaced_red = C.starting_pieces;
    unplaced_blue = C.starting_pieces
  }

  let current_player b =
    b.current_player

  let turn_count b =
    b.turn_count

  let active_pieces b =
    List.map (fun (s,p) -> (coord_to_ints s,p)) b.active_pieces

  let captured_pieces b c =
    choose c b.graveyard_red b.graveyard_blue

  (* r is the rank of the piece being added to graveyard g *)
  let add_to_graveyard r g =
    let new_count = match List.assoc_opt r g with
      | None -> 1
      | Some i -> i + 1 in
    (r,new_count)::(List.remove_assoc r g)

  (* Moving a piece into an unoccupied space is simpler than moving a
   * piece into an already-occupied space. *)
  let move_to_empty b c1 c2 =
    let moving = List.assoc c1 b.active_pieces in
    let updated = {moving with v = (match moving.v with
        | Revealed -> Revealed
        | _ -> Moved)} in
    let active_pieces' =
      (c2,updated)::(List.remove_assoc c1 b.active_pieces) in
    let turn_count' = match b.current_player with
      | Red -> b.turn_count
      | Blue -> b.turn_count + 1 in
    let moves_without_battle' = b.moves_without_battle + 1 in
    (* discards old history using trim *)
    let history_red' = trim (match b.current_player with
      | Red -> (c1,c2)::b.history_red
      | Blue -> b.history_red) in
    let history_blue' = trim (match b.current_player with
      | Red -> b.history_blue
      | Blue -> (c1,c2)::b.history_blue) in
    let current_player' = opposite b.current_player in {
      active_pieces = active_pieces';
      graveyard_red = b.graveyard_red;
      graveyard_blue = b.graveyard_blue;
      turn_count = turn_count';
      moves_without_battle = moves_without_battle';
      history_red = history_red';
      history_blue = history_blue';
      current_player = current_player'
    }

  (* It's easier to implement movement in two separate functions than to
   * check repeatedly whether the destination contains a piece. *)
  let move_to_occupied b c1 c2 =
    let moving = List.assoc c1 b.active_pieces in
    let target = List.assoc c2 b.active_pieces in
    let outcome = battle moving target in
    let survivor = match outcome with
      | Win -> Some moving
      | Tie -> None
      | Loss -> Some target in
    let without_m_t = List.remove_assoc c1
        (List.remove_assoc c2 b.active_pieces) in
    let active_pieces' = match survivor with
      | None -> without_m_t
      | Some p -> (c2,{p with v = Revealed})::without_m_t in
    let graveyard_red' = match b.current_player with
      | Red -> begin match outcome with
          | Win -> b.graveyard_red
          | _ -> add_to_graveyard moving.r b.graveyard_red
        end
      | Blue -> begin match outcome with
          | Loss -> b.graveyard_red
          | _ -> add_to_graveyard target.r b.graveyard_red
        end in
    let graveyard_blue' = match b.current_player with
      | Red -> begin match outcome with
          | Loss -> b.graveyard_blue
          | _ -> add_to_graveyard target.r b.graveyard_blue
        end
      | Blue -> begin match outcome with
          | Win -> b.graveyard_blue
          | _ -> add_to_graveyard moving.r b.graveyard_blue
        end in
    let turn_count' = match b.current_player with
      | Red -> b.turn_count
      | Blue -> b.turn_count + 1 in
    (* trims lists to get rid of old history that isn't used *)
    let history_red' = trim (match b.current_player with
      | Red -> (c1,c2)::b.history_red
      | Blue -> b.history_red) in
    let history_blue' = trim (match b.current_player with
      | Red -> b.history_blue
      | Blue -> (c1,c2)::b.history_blue) in
    let current_player' = opposite b.current_player in {
      active_pieces = active_pieces';
      graveyard_red = graveyard_red';
      graveyard_blue = graveyard_blue';
      turn_count = turn_count';
      moves_without_battle = 0;
      history_red = history_red';
      history_blue = history_blue';
      current_player = current_player'
    }

  (**
   * This function performs the task of revealing a scout's rank when it
   * moves more than one space in a single turn.  The board produced by this
   * function differs from the original board only in the visibility status
   * of the scout to be moved (and the order of the elements of
   * [active_pieces], but that shouldn't affect the board's behavior).
   *
   * If the piece to be moved is not a scout or is not attempting to move
   * multiple spaces in a single turn, this function returns the original
   * board.
   *
   * This takes the start and end spaces of the move in int-pair form as
   * well as the start space in coord form so it does not need to calculate
   * them from scratch.
   *)
  let scout_reveal b s1 s2 c1 =
    let moving = List.assoc c1 b.active_pieces in
    if moving.r = Scout then
      let horizontal = (fst s1) - (fst s2) in
      let vertical = (snd s1) - (snd s2) in
      if (horizontal >= 2) || (horizontal <= -2)
         || (vertical >= 2) || (vertical <= -2) then
        let moving' = {moving with v = Revealed} in
        let active_pieces' =
          (c1,moving')::(List.remove_assoc c1 b.active_pieces) in
        {b with active_pieces = active_pieces'}
      else b
    else b

  let placed_pieces p c =
    let convert (c,r) = (coord_to_ints c, r) in
    let red = List.map convert p.placed_red in
    let blue =  List.map convert p.placed_blue in
    choose c red blue

  let pieces_left p c = choose c p.unplaced_red p.unplaced_blue

  (* [in_placement_area c s] is true if the space s is in a proper placement
   * area. c is the color and determines what is considered a proper
   * placement area; c-Red uses C.starting_rows, and c=Blue uses
   * C.size-C.starting_rows. *)
  let in_placement_area c s =
    match c with
    | Red -> fst s < C.starting_rows
    | Blue -> fst s >= C.size - C.starting_rows

  (*[is_in_bounds (r,c)] is true if the row r is at least 0 and less than
   * size, the number of spaces on the edge of the board, and c is at least
   * 0 and less than size. *)
  let is_in_bounds (r,c) =  r >= 0 && r < size && c >= 0 && c < size

  let can_place_piece p c r s =
    let remaining = List.assoc r (pieces_left p c) > 0 in
    let alreadyTaken = List.mem_assoc s (placed_pieces p c) in
    is_in_bounds s && in_placement_area c s && remaining && not alreadyTaken

  (* [remove_piece_from_placed_pieces placedPieces s acc] is the new
   * list of (space,piece) tuples, with the piece at space s not included
   * in acc.
   * placedPieces i the original list.
   *)
  let rec remove_piece_from_placed_pieces placedPieces s acc =
    match placedPieces with
    | [] -> acc
    | (piece,sp)::t when s=sp -> remove_piece_from_placed_pieces t s acc
    | (piece,sp)::t -> remove_piece_from_placed_pieces t s ((sp,piece)::acc)

(*[remove_piece_from_unplaced_pieces unplacedPieces rk acc] is
 * the new list of tuples (rank,i) with i decremented by 1 for the tuple
 * with rank r equal to rk. i represents the number of those pieces unplaced.*)
  let rec remove_piece_from_unplaced_pieces unplacedPieces rk acc =
    match unplacedPieces with
    | [] -> acc
    | (r,i)::t when r=rk ->
      remove_piece_from_unplaced_pieces t rk
        ((r,i-1)::acc)
    | (r,i)::t ->
      remove_piece_from_unplaced_pieces t rk ((r,i)::acc)

(*[get_type_of_unplaced_pieces preboard color] uses the preboard pboard
  to return pboard.unplaced_red if the color c is Red and pboard.placed_blue
  if c=Blue.*)
  let get_type_of_unplaced_pieces pboard c =
    if (c = Red) then pboard.unplaced_red
    else pboard.unplaced_blue

(*[get_type_of_placed_pieces preboard color] uses the preboard pboard
  to return pboard.placed_red if the color c is Red and pboard.placed_blue
  if c=Blue.*)
  let get_type_of_placed_pieces pboard c =
    if (c = Red) then pboard.placed_red
    else pboard.placed_blue

(*[get_new_preboard p c new_unplaced_pieces new_placed_pieces] is the
  preboard with the new list of (rank,int) tuples representing the updated
  unplaced pieces, new_unplaced_pieces, and the new list of (coord,rank)
  tuples representing the updated placed pieces, new_placed_pieces*)
  let get_new_preboard p c new_unplaced_pieces new_placed_pieces =
    if (c = Red) then {
      placed_red = new_placed_pieces;
      placed_blue = p.placed_blue;
      unplaced_red = new_unplaced_pieces;
      unplaced_blue = p.unplaced_blue
    }
    else {
      placed_red = p.placed_red;
      placed_blue = new_placed_pieces;
      unplaced_red = p.unplaced_red;
      unplaced_blue = new_unplaced_pieces
    }

  (**
   * [place_piece] places a piece of rank [r] and color [s] at location [s]
   * in [p], removing it from the preboard's list of unplaced pieces.
   * Precondition:
   * [can_place_piece p c s = true]
  *)
  let place_piece pboard c r sp =
    if (can_place_piece pboard c r sp = true) then
      let type_of_unplaced_pieces = get_type_of_unplaced_pieces pboard c in
      let new_unplaced_pieces =
        remove_piece_from_unplaced_pieces type_of_unplaced_pieces r [] in
      let type_of_placed_pieces = get_type_of_placed_pieces pboard c in
      let new_placed_pieces =
        ((ints_to_coord sp),r)::type_of_placed_pieces in
      get_new_preboard pboard c new_unplaced_pieces new_placed_pieces
    else
      failwith "Cannot Place Piece"

(*[find_rank_of_piece_removed placedPieces sp] is the rank of the piece
  at space sp. Pattern matching is done on placedPieces, the list of
  (coord,rank) tuples, and the coord_to_ints function is used to
  get the space from each coord and then check if it is equal to sp.*)
  let rec find_rank_of_piece_removed placedPieces sp =
    match placedPieces with
    | (coor,r)::t when (coord_to_ints coor)=sp -> r
    | (coor,r)::t -> find_rank_of_piece_removed t sp
    | _ -> failwith "not possible to find this piece from placed pieces list"

(*[update_unplaced_pieces unplacedPieces pieceRemovedRank acc] is the updated
  list of (rank,int) tuples with the int corresponding to the rank
  pieceRemovedRank decremented by 1. This represents placing a piece on the
  preboard and having to remove it from the list of unplaced pieces. *)
  let rec update_unplaced_pieces unplacedPieces pieceRemovedRank acc =
    match unplacedPieces with
    | [] -> acc
    | (r,i)::t when r=pieceRemovedRank ->
      update_unplaced_pieces t pieceRemovedRank ((r,i+1)::acc)
    | (r,i)::t -> update_unplaced_pieces t pieceRemovedRank ((r,i)::acc)

(*[remove_piece_with_space placedPieces sp acc] is the list of (coord,rank) tuples
  without the tuple representing the piece that was originally at space sp.*)
  let rec remove_piece_with_space placedPieces sp acc =
    match placedPieces with
    | [] -> acc
    | (coor,r)::t when (coord_to_ints coor)=sp ->
      remove_piece_with_space t sp acc
    | h::t -> remove_piece_with_space t sp (h::acc)

  (**
   * [remove_piece p c s] removes the piece at [s] from its location,
   * returning it to the preboard's list of unplaced pieces.
   * Postcondition:
   * If there is no piece at [s], then [remove_piece p c s = p].
  *)
  let remove_piece pboard c sp =
    try
      let type_of_unplaced_pieces = get_type_of_unplaced_pieces pboard c in
      let type_of_placed_pieces = get_type_of_placed_pieces pboard c in
      let rankOfPieceRemoved =
        find_rank_of_piece_removed type_of_placed_pieces sp in
      let new_unplaced_pieces =
        update_unplaced_pieces type_of_unplaced_pieces rankOfPieceRemoved [] in
      let new_placed_pieces =
        remove_piece_with_space type_of_placed_pieces sp [] in
      get_new_preboard pboard c new_unplaced_pieces new_placed_pieces
    with
    | Failure _ ->
      pboard

  let clear p c =
    choose c {
      placed_red = [];
      placed_blue = p.placed_blue;
      unplaced_red = C.starting_pieces;
      unplaced_blue = p.unplaced_blue
    } {
      placed_red = p.placed_red;
      placed_blue = [];
      unplaced_red = p.unplaced_red;
      unplaced_blue = C.starting_pieces
    }

  let start_game p =
    let placed_helper pl_lst col =
      List.map (fun (c,r) -> (c,{c = col; r = r; v = Hidden})) pl_lst in
    let placed =
      placed_helper p.placed_red Red @
      placed_helper p.placed_blue Blue in {
      active_pieces = placed;
      graveyard_red = []; graveyard_blue = [];
      turn_count = 0; moves_without_battle = 0;
      history_red = []; history_blue = [];
      current_player = Red
    }

  (* (John) This checks if a potential move violates the three move
   * rule.  It returns true if the move violates it.
   * If a move from c1 to c2 violates the rule, the most recent move will
   * be from c2 to c1, and the move before that will be from c1 to c2. *)
  let violates_3move b c1 c2 =
    let hist = choose b.current_player b.history_red b.history_blue in
    match hist with
    | h1::t1 when h1 = (c2,c1) -> begin match t1 with
        | h2::_ when h2 = (c1,c2) -> true
        | _ -> false
      end
    | _ -> false

(*[is_not_lakes s] is true if the space s is not one of the lake spaces.
 * False otherwise. *)
  let is_not_in_lakes s =
    not (List.mem s lakes)

  (*[get_piece_at_space b s] is the piece at space s in board b.*)
  let get_piece_at_space b s =
    let coord_of_space = ints_to_coord s in
    List.assoc coord_of_space b.active_pieces

(*[did_not_hit_a_piece b s] is true if the space s is not occupied by
 * a piece in board b. False otherwise. *)
  let did_not_hit_a_piece b s =
    let coord_of_space = ints_to_coord s in
    not (List.mem_assoc coord_of_space b.active_pieces)

(*[not_same_color b s1 s2] is true if the colors of the pieces at spaces
 * s1 and s2 are the same. False otherwise. *)
  let not_same_color b s1 s2 =
    let piece1 = get_piece_at_space b s1 in
    let piece2 = get_piece_at_space b s2 in
    let color1 = piece1.c in
    let color2 = piece2.c in
    color1 <> color2

  (*[add_or_subtract_vert (r1,c1) (r2,c2)] increments r1 by 1 if r1
   * is less than r2. Else, it decrements r1 by 1. The space with this update
   * is returned. *)
  let add_or_subtract_vert (r1,c1) (r2,c2) =
    if (r1 < r2)
    then (r1+1,c1)
    else (r1-1,c1)

  (*[no_piece_in_between_vert b s1 s2] is true if there
   *are no pieces in between the spaces s1 and s2 in board b. false otherwise.*)
  let rec no_piece_in_between_vert b sOrig s1 s2 =
    if (s1=s2)
    then true
    else if (((List.mem_assoc (ints_to_coord s1) b.active_pieces)
             && s1<>sOrig)||(List.mem s1 lakes))
    then
      false
    else
      no_piece_in_between_vert b sOrig (add_or_subtract_vert s1 s2) s2

(*[add_or_subtract_horiz (r1,c1) (r2,c2)] increments c1 by 1 if c1
 * is less than c2. Else, it decrements c1 by 1. The space with this update
 * is returned. *)
  let add_or_subtract_horiz (r1,c1) (r2,c2) =
    if (c1 < c2)
    then (r1,c1+1)
    else (r1,c1-1)

(*[no_piece_in_between_horiz b s1 s2] is true if there
 * are no pieces in between the spaces s1 and s2 in board b. false otherwise.*)
  let rec no_piece_in_between_horiz b sOrig s1 s2 =
    if (s1=s2)
    then true
    else if (((List.mem_assoc (ints_to_coord s1) b.active_pieces) &&
             s1<>sOrig)||(List.mem s1 lakes))
    then false
    else no_piece_in_between_horiz b sOrig (add_or_subtract_horiz s1 s2) s2

(*[can_make_move_with_piece b p s1 s2] determines whether a piece can make
 * a certain move from s1 to s2. Checks that the piece moves
 *  without jumping over pieces (only an issue for scouts) and
 * that it only moves one space to the right/left or up/down (no diagonals)
 *  if it is not a scout or a Bomb or a Flag.*)
  let can_make_move_with_piece (b:board) (p:piece) (s1:space) (s2:space) =
    match (p.r,s1,s2) with
    | (r,_,_) when (r=Flag || r=Bomb) -> false
    | (r,(a1,b1),(a2,b2))
      when (r=Scout && ((((abs (a1-a2) >= 1)&&(b1-b2=0))&&
                         (no_piece_in_between_vert b (a1,b1) (a1,b1) s2)))) -> true
    | (r,(a1,b1),(a2,b2))
      when  (r=Scout && ((((abs (b1-b2) >= 1)&&(a1-a2=0))&&
                          (no_piece_in_between_horiz b (a1,b1) (a1,b1) s2)))) -> true
    | (_,(a1,b1),(a2,b2))
      when (((abs (a1-a2) = 1)&&(b1-b2=0)) ||
            (((abs (a1-a2) = 1)&&(b1-b2=0)))) -> true
    | (_,(a1,b1),(a2,b2))
      when (((abs (b1-b2) = 1)&&(a1-a2=0)) ||
        (((abs (b1-b2) = 1)&&(a1-a2=0)))) -> true
    | _ -> false


  let is_valid_move b s1 s2 =
    if ((is_in_bounds s1) && (is_in_bounds s2) && (is_not_in_lakes s2) &&
        (did_not_hit_a_piece b s2 || not_same_color b s1 s2)
        && not(violates_3move b (ints_to_coord s1) (ints_to_coord s2)))
    then
      let pieceAtS1 = get_piece_at_space b s1 in
      let canMakeMove = can_make_move_with_piece b pieceAtS1 s1 s2 in
      canMakeMove
    else
      false

  let move b s1 s2 =
    if is_valid_move b s1 s2 then () else failwith "Invalid Move";
    let c1 = ints_to_coord s1 in
    let c2 = ints_to_coord s2 in
    (* use scout_reveal here; it has no effect in most circumstances *)
    let b' = scout_reveal b s1 s2 c1 in
    match List.assoc_opt c2 b'.active_pieces with
    | None -> move_to_empty b' c1 c2
    | Some _ -> move_to_occupied b' c1 c2


  (* [get_mobile_spaces_of_color_helper a c acc] returns a list of the spaces
   * in [a]'s pairs occupied by moveable (neither flags nor bombs) pieces of
   * color [c] *)
  let rec get_mobile_spaces_of_color_helper activePieces c acc =
    match activePieces with
    | [] -> acc
    | (coor,p)::t when (p.c = c) && (p.r <> Flag && p.r <> Bomb)->
      get_mobile_spaces_of_color_helper t c ((coord_to_ints coor)::acc)
    | _::t -> get_mobile_spaces_of_color_helper t c acc

  (* [get_mobile_spaces_of_color b] returns a list of the spaces in b
   * occupied by moveable (neither flags nor bombs) pieces of the current
   * player *)
  let get_mobile_spaces_of_color b =
    let c = b.current_player in
    get_mobile_spaces_of_color_helper b.active_pieces c []

  (*[get_move_col_up (r,c) acc] adds the space that is a col to the right of
   * (r,c) to acc if it is a valid move, and simply keeps the original acc
   * if not. acc is then returned.*)
  let get_move_col_up b (r,c) acc =
    if (is_valid_move b (r,c) (r,c+1))
    then (r,c+1)::acc
    else acc

  (*[get_move_col_down (r,c) acc] adds the space that is a col to the left of
   * (r,c) to acc if it is a valid move, and simply keeps the original acc
   * if not. get_move_col_up is called to check if the col to the right of (r,c)
   * is valid.*)
  let get_move_col_down b (r,c) acc =
    if (is_valid_move b (r,c) (r,c-1))
    then get_move_col_up b (r,c) ((r,c-1)::acc)
    else get_move_col_up b (r,c) acc

  (*[get_move_row_up b (r,c) acc] adds the space that is a row
   * above (r,c) to acc if it is a valid move, and simply keeps the original acc
   * if not.get_move_col_down is called to check if the col to the left of (r,c)
   * is valid.*)
  let get_move_row_up b (r,c) acc =
    if (is_valid_move b (r,c) (r+1,c))
    then get_move_col_down b (r,c) ((r+1,c)::acc)
    else get_move_col_down b (r,c) acc

(*[get_move_row_down b (r,c) acc] adds the space that is a row
 * below (r,c) to acc if it is a valid move, and simply keeps the original acc
 * if not. get_move_row_up is called to check if the row above (r,c) is
 * valid.*)
  let get_move_row_down b (r,c) acc =
    if (is_valid_move b (r,c) (r-1,c))
    then get_move_row_up b (r,c) ((r-1,c)::acc)
    else get_move_row_up b (r,c) acc

(*[get_moves_of_other_pieces b s p] gets the possible moves of pieces that
 * are not the scout or the bomb or the flag. First, calls a helper function to
 * check if the piece can move a row down.*)
  let get_moves_of_other_pieces b s p =
    get_move_row_down b s []

  (*[rec_get_moves_cols_up b (r1,c1) (r2,c2) acc] gets the spaces that are
   * columns to the right of the initial space, (r1,c1), and puts those spaces
   * in acc. Once an invalid move is reached, acc is returned. *)
  let rec rec_get_moves_cols_up b (r1,c1) (r2,c2) acc =
    if (is_valid_move b (r1,c1) (r2,c2))
    then rec_get_moves_cols_up b (r1,c1) (r2,c2+1) ((r2,c2)::acc)
    else acc

  (*[rec_get_moves_cols_down b (r1,c1) (r2,c2) acc] gets the spaces that are
   * columns to the left of the initial space, (r1,c1), and puts those spaces
   * in acc. Once an invalid move is reached, rec_get_moves_cols_up is called
   * to get the spaces to the right of  (r1,c1) that the scout can move to. *)
  let rec rec_get_moves_cols_down b (r1,c1) (r2,c2) acc =
    if (is_valid_move b (r1,c1) (r2,c2))
    then rec_get_moves_cols_down b (r1,c1) (r2,c2-1) ((r2,c2)::acc)
    else rec_get_moves_cols_up b (r1,c1) (r2,c1+1) acc

  (*[rec_get_moves_rows_up b (r1,c1) (r2,c2) acc] gets the spaces
   * that are rows above the initial space, (r1,c1), and puts those spaces
   * in acc. Once an invalid move is reached, rec_get_moves_cols_down is called
   * to get the spaces to the left of (r1,c1) that the scout can move to. *)
  let rec rec_get_moves_rows_up b (r1,c1) (r2,c2) acc =
    if (is_valid_move b (r1,c1) (r2,c2))
    then rec_get_moves_rows_up b (r1,c1) (r2+1,c2) ((r2,c2)::acc)
    else rec_get_moves_cols_down b (r1,c1) (r1,c1-1) acc

(*[rec_get_moves_rows_down b (r1,c1) (r2,c2) acc] gets the spaces
 * that are rows below the initial space, (r1,c1), and puts those spaces
 * in acc. Once an invalid move is reached, rec_get_moves_rows_up is called
 * to get the spaces above (r1,c1) that the scout can move to. *)
  let rec rec_get_moves_rows_down b (r1,c1) (r2,c2) acc =
    if (is_valid_move b (r1,c1) (r2,c2))
    then rec_get_moves_rows_down b (r1,c1) (r2-1,c2) ((r2,c2)::acc)
    else rec_get_moves_rows_up b (r1,c1) (r1+1,c2) acc

(*[get_moves_scout b s] gets the possible moves of a scout at space
 * s and board b. Calls a helper function to first check how many rows
 * down the scout can go. *)
  let get_moves_of_scout b s =
    rec_get_moves_rows_down b s s []

(*[get_moves_of_piece b s p] gets the possible moves of the piece
 * p at space s, in board b. If the piece's rank is a Bomb or a Flag,
 * then an empty list is returned. Else, helper functions are called to
 * do the logic for determining the possible moves. *)
  let get_moves_of_piece b s p =
    match p.r with
    | Scout -> get_moves_of_scout b s
    | Bomb -> []
    | Flag -> []
    | _ -> get_moves_of_other_pieces b s p

  (* [get_values_of_keys b ml acc] returns a list of space pairs. where
   * the first value is a space in [ml] and the second value is a space
   * where the piece in [ml] may move. This function should return a
   * tuple for each possible space the pieces represented in mobileSapceList
   * could move.
   * Precondition: Neither a bomb nor a flag is in a space in [ml] *)
  let rec get_values_of_keys b mobileSpacesList acc =
    match mobileSpacesList with
    | [] -> acc
    | s::t ->
      let coordOfSpace = ints_to_coord s in
      let pieceAtCoord = List.assoc coordOfSpace b.active_pieces in
      let possibleMovesList = get_moves_of_piece b s pieceAtCoord in
      if (pieceAtCoord.r=Flag || pieceAtCoord.r=Bomb)
      then get_values_of_keys b t acc
      else get_values_of_keys b t ((s,possibleMovesList)::acc)

  let all_moves b =
    let mobileSpacesOfMyColor = get_mobile_spaces_of_color b in
    let superset = get_values_of_keys b mobileSpacesOfMyColor [] in
    List.filter (fun (_,l) -> l <> []) superset

(*[status_of_game b] is the status of the game with board b. A variant
 * was made to represent the status, with constructors Timeout, Finished Blue,
 * Finished Red, Finished, and Ongoing. If the timeout limit is exceeded,
 * represented by C.timeout_limit, then Timeout is returned. If the Flag
 * is in the red or the blue graveyards, then Finished Blue or
 * Finished Red are returned, respectively. If one player has
 * no more moves, determined from the all_moves function, then the opposing
 * player wins. Else, Ongoing is returned. *)
  let status_of_game b =
    if b.moves_without_battle >= C.timeout_limit then Timeout
    else if List.mem_assoc Flag b.graveyard_red then Finished Blue
    else if List.mem_assoc Flag b.graveyard_blue then Finished Red
    else if all_moves b = [] then Finished (opposite b.current_player)
    else Ongoing

  (**
   * This produces a record of information about the last move in the event
   * that the last move was a battle that ended in a tie.  This takes the
   * start and end spaces of the move as input so it does not need to
   * find their values over again after [last_move] finds them.
   *)
  let last_move_for_tie b ss es =
    let (rank_of_moved,_) = List.hd
        (choose b.current_player b.graveyard_blue b.graveyard_red) in
    let (rank_of_defender,_) = List.hd
        (choose b.current_player b.graveyard_red b.graveyard_blue) in
    let m = {
      r = rank_of_moved;
      c = opposite b.current_player;
      v = Revealed
    } in
    let d = {
      r = rank_of_defender;
      c = b.current_player;
      v = Revealed
    } in {
      start_space = ss;
      end_space = es;
      moved = m;
      defender = Some d;
      outcome = Some Tie
    }

  (**
   * This is used for getting information about the last move when that
   * move was a battle that did not end in a tie.
   *)
  let last_move_for_battle b ss es survivor =
    let (rank_of_opponent,_) = List.hd
        (choose (opposite survivor.c) b.graveyard_red b.graveyard_blue) in
    let opp = {
      r = rank_of_opponent;
      c = opposite survivor.c;
      v = Revealed
    } in
    (* determine which piece was the one that moved as well as the outcome
     * of the battle using the fact that the current player's color is the
     * color of the defender *)
    let (m,d,oc) = if survivor.c = b.current_player
      then (opp,survivor,Loss)
      else (survivor,opp,Win) in {
      start_space = ss;
      end_space = es;
      moved = m;
      defender = Some d;
      outcome = Some oc
    }

  let last_move b =
    let (sc,ec) = List.hd (choose b.current_player
                             b.history_blue b.history_red) in
    let (ss,es) = (coord_to_ints sc,coord_to_ints ec) in
    if b.moves_without_battle = 0 then
      let survivor = List.assoc_opt ec b.active_pieces in
      match survivor with
      | None -> last_move_for_tie b ss es
      | Some p -> last_move_for_battle b ss es p
    else {
      start_space = ss;
      end_space = es;
      moved = List.assoc ec b.active_pieces;
      defender = None;
      outcome = None
    }

end

module Board8 = MakeBoard (Constants8)

module Board10 = MakeBoard (Constants10)
