open Board


(* [AIMake(B:Board)] represents the functionality of an AI opponent in
 * a match of Stratego for Board type [B]*)
module type AIMaker =
  functor(B:Board) -> sig
    (**
     * These are the board and preboard types from the Board module that this
     * specific BoardAI uses.
    *)
    type preboard = B.preboard
    type board = B.board

    (**
     * [auto_setup p c] places all pieces of color [c] at valid locations in
     * [p].
     * Postcondition:
     * The locations of pieces of color [c] that have been placed already
     * may change.
     * The placements of the pieces of the opposite color will not be
     * changed. Also, the AI will not take the placements of the pieces of
     * the opposite color into consideration when it places the pieces of
     * color [c].  The output of this function may be randomized.
     *)
    val auto_setup : color -> preboard-> preboard

    (**
     * [choose_move b] selects a move for the current player to take.
     * Precondition:
     * [status_of_game b = Ongoing]
     * [all_moves b] contains at least one element.
     * [B.current_player b] corresponds to the AI's color
     * Postcondition:
     * The left entry of the returned tuple is the location of the piece to
     * move, and the right entry is the space to which the piece should be
     * moved.  This tuple is guaranteed to represent a valid move for [b].
     * The output of this function may be randomized.
    *)
    val choose_move : board -> (space * space)
  end
module MakeBoardAI : AIMaker
