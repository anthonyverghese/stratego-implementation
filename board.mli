(**
 * The colors for the two players.
*)
type color = Red | Blue

(**
 * The status of a game indicates whether it has finished.
 * Ongoing corresponds to a game that has not yet completed.
 * Timeout corresponds to a tie (50 moves without a single capture).
 * Finished corresponds to a particular player (color) winning.
 *)
type status = Ongoing | Timeout | Finished of color

(**
 * This indicates the result of a battle between two pieces.  Win means that
 * the attacker wins, Loss means that the attacker loses, and Tie means that
 * both peices lose and are removed from play.
 *)
type battle_outcome = Win | Loss | Tie

(**
 * The client (i.e. the command line and GUI files) can only use integer
 * incides for rows and columns.  The left entry is the row and the right
 * entry is the column.
 *)
type space = int * int

(**
 * These are the twelve types of Stratego pieces.  This variant needs to be
 * externally visible so that the command line and the GUI can know how to
 * display the state of a board visually.
 *)
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

(* The user interface and the AI need information about the visibility of
 * pieces.  At the start of the game, all pieces are Hidden.  When a piece
 * moves for the first time, it becomes Moved.  When a piece attacks another
 * piece or defends against another piece, it becomes Revealed and stays that
 * way for the rest of the game (until it is captured).  This type needs to
 * be externally visible for the same reason that pclass needs to be. *)
type visibility = Hidden | Moved | Revealed

(**
 * This is how pieces are represented.  Once again, this needs to be
 * visible from the outside for purposes of visual representation.
 *)
type piece = {
  c: color;
  r: rank;
  v: visibility
}

(**
 * This type allows both the user interface to provide the user with
 * information about the last move that occurred in a game.
 *)
type move_info = {
  start_space: space;
  end_space: space;
  moved: piece;
  defender: piece option;
  outcome: battle_outcome option
}

(* The MakeBoard functor does not appear in the .mli file. *)

module type Board = sig

  (**
   * The internal representation of the game itself after setup.
   *)
  type board

  (**
   * This type is used for the setup of each player's pieces before a game
   * begins.
   *)
  type preboard

  (**
   * Each player will have a certain number of spaces to which he would be
   * able to place pieces in the initial phase of the game.  [starting_rows]
   * is that number.
   *)
  val starting_rows: int

  (**
   * Each board contains a certain number of rows that does
   * not change over the course of the game or between different games.
   * This integer returns this number of rows/columns.
   *)
  val size : int

  (**
   * Each board contains a certain number of impassable lake tiles that do
   * not change over the course of the game or between different games.
   * This list represents their locations.
   *)
  val lakes : space list

  (**
   * The number of pieces of each rank with which each player starts.
   *)
  val starting_pieces : (rank * int) list

  (**
   * A preboard in which no piece has been placed yet.
   *)
  val empty : preboard

  (**
   * [placed_pieces p c] returns an association list of the pieces of color
   * [c] that have been placed into [p], with their locations as keys.
   *)
  val placed_pieces : preboard -> color -> (space * rank) list

  (**
   * [pieces_left p c] returns a list of the pieces of color [c] that have
   * not been placed into [p] yet.
   *)
  val pieces_left : preboard -> color -> (rank * int) list

  (**
   * [can_place_piece p c s] indicates whether a piece of rank [r] and color
   * [c] can be placed at location [s] in [p].
   * Postcondition:
   * If this returns false, it is for one of the following reasons:
   * -Every piece of rank [r] and color [c] has been placed already.
   * -[s] is a lake tile.
   * -Another piece is already at [s].
   * -[s] is not within the [c] player's placement area.
   *)
  val can_place_piece : preboard -> color -> rank -> space -> bool

  (**
   * [place_piece] places a piece of rank [r] and color [s] at location [s]
   * in [p], removing it from the preboard's list of unplaced pieces.
   * Precondition:
   * [can_place_piece p c s = true]
   *)
  val place_piece : preboard -> color -> rank -> space -> preboard

  (**
   * [remove_piece p c s] removes the piece at [s] from its location,
   * returning it to the preboard's list of unplaced pieces.
   * Postcondition:
   * If there is no piece at [s], then [remove_piece p c s = p].
   *)
  val remove_piece : preboard -> color -> space -> preboard

  (**
   * Resets all pieces of the given color, but leaves the pieces of the
   * opposite color in their current configuration.
   *)
  val clear : preboard -> color -> preboard

  (**
   * Precondition:  Every piece in the given preboard has been placed at a
   * location.  This raises an exception if any piece has not been placed.
   * Postcondition:  The pieces in the board produced appear at the same
   * locations in which they appear in the preboard.  Also, the new board
   * has a turn count of 0, and the current player is Red.
   *)
  val start_game : preboard -> board

  (**
   * The only way to advance the board to a new state is to move a piece of
   * this color.
   *)
  val current_player : board -> color

  (**
   * A single "turn" consists of a red move followed by a blue move.  this
   * increases by 1 after every move that the blue player makes.
   *)
  val turn_count : board -> int

  (**
   * This allows a client to see when and how a game has finished.
   *)
  val status_of_game : board -> status

  (**
   * Produces an association list of all the pieces currently active in the
   * game and their locations.  Not all of the information that this
   * function returns should be shown to the user, since players hide the
   * values of their pieces from each other.
   *)
  val active_pieces : board -> (space * piece) list

  (**
   * This allows the client to provide both the user and the AI with
   * information about the pieces that have been captured so far.  Keeping
   * track of these pieces is an important part of the game.
   * Previously, the type of this function was board -> (rank * int) list.
   * The UI needs to display the captured pieces for each color separately,
   * so this new type prevents the UI from needing to filter through a
   * list of captured pieces of both colors.
   *)
  val captured_pieces : board -> color -> (rank * int) list

  (**
   * [all_moves b] produces a list of all moves that the current player can
   * make at this time.
   * Precondition:
   * [status_of_game b = Ongoing]
   * Postcondition:
   * The left entries of the tuples in the returned list are the locations
   * of the pieces that can move.  The right entries are the lists of
   * spaces to which those pieces can move.  If a piece cannot move in the
   * current state of the board, it will not be included in the list.
   *)
  val all_moves : board -> (space * space list) list

  (**
   * [is_valid_move b start end] indicates whether the current player can
   * move the piece at [start] to the space [end].
   * Precondition:
   * [status_of_game b = Ongoing]
   * Postcondition:
   * This is equivalent to
   * [List.mem end (List.assoc start (all_moves b))].
   *)
  val is_valid_move : board -> space -> space -> bool

  (**
   * [move b start end] moves the piece at [start] to the space [end],
   * changes the current player to the opposite color, and, if the player
   * whose piece was moved was Blue, increases the turn count by 1.
   * Precondition:
   * [status_of_game b = Ongoing]
   * [is_valid_move b start end = true]
   * Postcondition:
   * This may change the status of the game to Timeout or Finished.
   *)
  val move : board -> space -> space -> board

  (**
   * [last_move b] returns a record containing information about the last
   * move that the current player's opponent made.  This includes the start
   * and end positions of the piece that moved, the piece that moved, and
   * the piece that it attacked, if any.
   * Precondition:
   * At least one move has occurred so far in the given board.
   * Postcondition:
   * The [defender] and [outcome] fields of the returned move_info are
   * either both None or both Some.
   *)
  val last_move : board -> move_info

end

module Board8: Board

module Board10: Board
