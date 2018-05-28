open Board

module type AIMaker =
  functor(B:Board) -> sig
    type preboard = B.preboard
    type board = B.board
    val auto_setup : color -> preboard-> preboard
    val choose_move : board -> (space * space)
  end

module MakeBoardAI : AIMaker =
  functor(B : Board) -> struct
    type preboard = B.preboard
    type board = B.board


(* Checks to see if there are pieces of a particular rank and color
   that haven't been placed yet *)
  let containsPieceType preboard color rank =
    let piecesLeft = B.pieces_left preboard color in
    List.fold_left
      (fun hasPiece (r, num) ->
         if r = rank then hasPiece || true else hasPiece) false piecesLeft

(* Place a piece randomly on the board. Outputs a new board with that
   piece placed *)
    let rec placePieceRandom preboard color pieceRank pieceTypeLeft =
    if pieceTypeLeft > 0 then
       let coordinateRow =
        if color = Red then
          Random.int (B.size/2)
        else
          B.size - 1 - (Random.int (B.size/2)) in
       let coordinateCol = Random.int B.size in
       let coordinatePair = (coordinateRow, coordinateCol) in
       if (B.can_place_piece preboard color pieceRank coordinatePair) then
         let piecePlaced = B.place_piece preboard color pieceRank
             coordinatePair in
         placePieceRandom piecePlaced color pieceRank (pieceTypeLeft - 1)
       else
         placePieceRandom preboard color pieceRank pieceTypeLeft
    else
       preboard

(* For every piece passed in, place that piece on the board using
   placePieceRandom *)
    let rec placeAllPiecesRandom preboard color piecesLeft =
      match piecesLeft with
      | [] -> preboard
      | (pieceRank, numLeft) :: t ->
        let piecesOfSpecificRankPlaced =
          placePieceRandom preboard color pieceRank numLeft in
        placeAllPiecesRandom piecesOfSpecificRankPlaced color t

(* Place pieces of the specified type and color in the backrow during
   the setup phase *)
    let rec placePieceBackRows preboard color pieceRank pieceTypeLeft =
      if pieceTypeLeft > 0 then
        let coordinateRow =
          if color = Red then
            Random.int ((B.size/2) - 2)
          else
            B.size - 1 - (Random.int ((B.size/2) - 2)) in

        let coordinateCol = Random.int B.size in
        let coordinatePair = (coordinateRow, coordinateCol) in
        if (B.can_place_piece preboard color pieceRank coordinatePair) then
          let piecePlaced = B.place_piece preboard color pieceRank
              coordinatePair in
          placePieceBackRows piecePlaced color pieceRank (pieceTypeLeft - 1)
        else
          placePieceBackRows preboard color pieceRank pieceTypeLeft
      else
        preboard

    let rec placeBombs_helper preboard color bombsLeft =
      if bombsLeft > 0 then
        let (flagX, flagY) =
          match List.find_opt (fun (s,r) -> r = Flag)
                  (B.placed_pieces preboard color) with
          | Some(space, _)-> space
          | None ->
            failwith "placeBombs_helper did not find the location of the flag"
        in
        let coordinateRow =
          if Random.bool () then
            flagX + (Random.int ((B.size/2)+1))
          else
            flagX - (Random.int ((B.size/2)+1))
        in
        let coordinateCol =
          if Random.bool () then
            flagY + Random.int 2
          else
            flagY - Random.int 2
        in
        if (B.can_place_piece preboard color Bomb
              (coordinateRow, coordinateCol)) then
          let bombPlaced = B.place_piece preboard color Bomb
              (coordinateRow, coordinateCol) in
          placeBombs_helper bombPlaced color (bombsLeft - 1)
        else
          placeBombs_helper preboard color bombsLeft
      else
        preboard

(* Place bombs within a certain range of where the flag is. This function
   assumes that the flag has already been placed *)
    let placeBombs preboard color =
      let bombsLeft = List.assoc Bomb (B.pieces_left preboard color) in
      placeBombs_helper preboard color bombsLeft


    let rec placeScouts_helper preboard color scoutsLeft =
      if scoutsLeft > 0 then
        let coordinateRow =
          if (Random.float 1.) >= 0.75 then
            if color = Red then
              Random.int ((B.size/2)-2)
            else
              B.size - 1 - (Random.int ((B.size/2)-2))
          else
            if color = Red then
              Random.int ((B.size/2)-2)
            else
              B.size - (Random.int ((B.size/2)-2))
        in
        let coordinateCol =
          Random.int B.size
        in
        if (B.can_place_piece preboard color Scout
              (coordinateRow, coordinateCol)) then
          let scoutPlaced = B.place_piece preboard color Scout
              (coordinateRow, coordinateCol) in
          placeScouts_helper scoutPlaced color (scoutsLeft - 1)
        else
          placeScouts_helper preboard color scoutsLeft
      else
        preboard

(* Place scouts on the board with a 75% chance of one of them
   being placed in the front and a 25% chance of one of them
   being placed in the back *)
    let placeScouts preboard color =
      let scoutsLeft = List.assoc Scout (B.pieces_left preboard color) in
      placeScouts_helper preboard color scoutsLeft

(* Have the AI setup its own board. The order of setup is the flag, bombs,
   scouts, miners and then the rest in an unspecified order. *)
    let auto_setup c pb=
      let () =
        Random.init (abs((Unix.time () |> int_of_float) mod 100000000)) in
      let pbClear = B.clear pb c in
      let flagPlaced = placePieceBackRows pbClear c Flag 1 in
      let bombsPlaced = placeBombs flagPlaced c in
      let scoutsPlaced = placeScouts bombsPlaced c in
      let minersLeft = List.assoc Miner (B.pieces_left scoutsPlaced c) in
      let minersPlaced =
        placePieceBackRows scoutsPlaced c Miner minersLeft in
      placeAllPiecesRandom minersPlaced c (B.pieces_left minersPlaced c)


    let distanceBetween (x1, y1) (x2, y2) =
      let xDiff = abs (x1 - x2) in
      let yDiff = abs (y1 - y2) in
      xDiff + yDiff


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


(* List of tuples that contain space and piece details that is filtered by
   whether they are a revealed enemy *)
    let revealedEnemies board =
      let enemyColor =
        if B.current_player board = Red then Blue
        else Red
      in
      List.filter (fun (tile, pieceDetails) ->
          match pieceDetails with
          | { c = ec ; r = _ ; v = Revealed} when ec = enemyColor
            -> true
          | _ -> false
        ) (B.active_pieces board)

    (* Cycle through the list of moves in all_moves and pick a random move *)
    let choose_move_random board =
      let moveOptions = B.all_moves board in
      let numPieces = List.length moveOptions in
      let selectedPiece = List.nth moveOptions (Random.int numPieces) in
      let numPieceMoves = List.length (snd selectedPiece) in
      let moveSelected = List.nth (snd selectedPiece)
          (Random.int numPieceMoves) in
      ((fst selectedPiece), moveSelected)


(* Filter through list of moves based on an enemy target and all ally
   pieces' moves where the pieces are of higher rank than the target *)
    let higherRankedMoves board =
      let enemyTarget = List.hd (revealedEnemies board) in
      if (snd enemyTarget).r = Flag then
        B.all_moves board
      else
      if (snd enemyTarget).r = Bomb then
        List.filter (fun (sp1, sp2) ->
            (List.exists (fun (currSp, pieceDetails) ->
                 sp1 = currSp && pieceDetails.r = Miner)
                (B.active_pieces board)))
          (B.all_moves board)
      else
        List.filter (fun (sp1, sp2) ->
            (List.exists (fun (currSp, pieceDetails) ->
                 sp1 = currSp && (List.mem pieceDetails.r
                                    (can_capture (snd enemyTarget))))
               (B.active_pieces board)))
          (B.all_moves board)

(* Choose a move randomly if there are no revealed pieces or if you have
   no higher ranked pieces. Otherwise, pick a target piece and move a higher
   ranked piece towards that piece *)
    let choose_move board =
      try
        if (List.length (revealedEnemies board)) = 0 then
          choose_move_random board
        else
  (* Gets all of the moves for the pieces that can capture the target piece *)
          let moveOptions = higherRankedMoves board in
          let pieceSelected = List.nth moveOptions
              (Random.int (List.length moveOptions)) in
          let enemyTargetLoc = fst (List.hd (revealedEnemies board)) in
          let destSpace = List.fold_left (fun acc resultOfMove ->
              if (distanceBetween enemyTargetLoc resultOfMove) <
                 (distanceBetween enemyTargetLoc acc) then
                resultOfMove
              else
                acc
            ) (9999, 9999) (snd pieceSelected) in
          ((fst pieceSelected), destSpace)
      with
      | _ -> choose_move_random board

    end

module AI8 = MakeBoardAI(Board8)
module AI10 = MakeBoardAI(Board10)
