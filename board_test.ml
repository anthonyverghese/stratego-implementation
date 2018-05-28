open OUnit2
open Board
open Ai

module GeneralTests (B : Board) = struct

  module A = MakeBoardAI (B)

  (* If all pieces of a given rank have been placed already, that rank
   * will still appear as a key in [pieces_left], but its corresponding
   * value will be 0. *)
  let all_placed = List.map (fun (r,_) -> (r,0)) B.starting_pieces

  (* several tests use a preboard with both sides set automatically *)
  let pre = A.auto_setup Blue (A.auto_setup Red B.empty)

  (* automatically generated board *)
  let brd = B.start_game pre

  (* sorted version of starting piece count *)
  let sorted_pieces = List.sort Pervasives.compare B.starting_pieces

  (* This function checks the number of pieces of each rank of a given
   * color that are represented in a board either as active pieces or as
   * captured pieces.  If those numbers do not equal the numbers in
   * [B.starting_pieces], this fails.  The last test case uses this. *)
  let check_piece_count b c =
    let active_of_c = List.map snd
        (List.filter (fun (_,p) -> p.c = c) (B.active_pieces b)) in
    let captured_of_c = (B.captured_pieces b c) in
    let rec assemble pieces acc =
      match pieces with
      | [] -> acc
      | h::t -> begin match List.assoc_opt h.r acc with
          | None -> assemble t ((h.r,1)::acc)
          | Some n -> let acc' = List.remove_assoc h.r acc in
            assemble t ((h.r,n+1)::acc')
        end in
    let sorted_of_c = List.sort Pervasives.compare
        (assemble active_of_c captured_of_c) in
    assert_equal sorted_of_c sorted_pieces

  (* general test cases go in this list *)
  let tests = [

    (* Examines the properties of the empty preboard *)
    "empty_preboard" >:: (fun _ ->
        assert_equal (B.placed_pieces B.empty Red) [];
        assert_equal (B.placed_pieces B.empty Blue) []);

    (* Auto setup leaves no pieces unplaced *)
    "auto_setup1" >:: (fun _ ->
        assert_equal all_placed (B.pieces_left pre Red);
        assert_equal all_placed (B.pieces_left pre Blue));

    (* Applying auto setup multiple times for one color does not change the
     * fact that all pieces of that color have been placed *)
    "auto_setup2" >:: (fun _ ->
        let p = A.auto_setup Red B.empty in
        assert_equal (B.pieces_left p Red)
          (B.pieces_left (A.auto_setup Red p) Red));

    (* Auto setup does not change pieces of the opposite color *)
    "auto_setup3" >:: (fun _ ->
        let p = A.auto_setup Red B.empty in
        assert_equal B.starting_pieces (B.pieces_left p Blue));

    (* Auto setup does not put pieces in lake locations *)
    "auto_setup4" >:: (fun _ ->
      ignore (List.fold_left (fun _ (c,p)->
          if (List.mem c B.lakes)
          then failwith "AI placed piece in lake"
          else true) true
          ((B.placed_pieces pre Red) @ (B.placed_pieces pre Blue))));

    (* Auto setup does not place multiple pieces at the same location *)
    "auto_setup5" >:: (fun _ ->
        let locs = (B.placed_pieces pre Red) @ (B.placed_pieces pre Blue) in
        let keys = List.map fst locs in
        let sorted = List.sort Pervasives.compare keys in
        let no_dups = List.sort_uniq Pervasives.compare keys in
        assert_equal sorted no_dups);

    (* Auto setup places red pieces only in the low-numbered rows and
     * places blue pieces only in the high-numbered rows *)
    "auto_setup6" >:: (fun _ ->
      let locs_r = B.placed_pieces pre Red in
      let rows_r = List.map (fun x -> fst (fst x)) locs_r in
      let invalid_r = List.filter
          (fun x -> (x >= B.starting_rows) || (x < 0))
          rows_r in
      assert_equal invalid_r [];
      let locs_b = B.placed_pieces pre Blue in
      let rows_b = List.map (fun x -> fst (fst x)) locs_b in
      let invalid_b = List.filter
          (fun x -> (x < B.size - B.starting_rows) || (x >= B.size))
          rows_b in
      assert_equal invalid_b []);

    (* Bombs never have Moved as their visibility, and Flags are always
     * Hidden *)
     "bomb_flag_immobile" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing
        do let ps = B.active_pieces !b in
          ignore (List.fold_left (fun _ (_,p) ->
              match p.r with
              | Bomb -> if p.v = Moved then failwith "Moved Bomb" else ()
              | Flag -> if p.v <> Hidden then failwith "Flag Visibility"
                else ()
              | _ -> ())
              () ps);
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* No two pieces occupy the same location at any point in the game *)
    "no_overlap" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing
        do let ps = B.active_pieces !b in
          let locs = List.map fst ps in
          let sorted = List.sort Pervasives.compare locs in
          let no_dups = List.sort_uniq Pervasives.compare locs in
          assert_equal sorted no_dups;
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* No pieces ever occupy lake tiles during the main game *)
    "no_pieces_in_lakes" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing
        do let ps = B.active_pieces !b in
          ignore (List.fold_left
                    (fun _ (s,_) -> assert_equal false (List.mem s B.lakes))
                    () ps);
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* No piece ever occupies an out-of-bounds location *)
    "within_bounds" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing
        do let ps = B.active_pieces !b in
          ignore
            (List.fold_left
               (fun _ ((r,c),_) -> assert_equal true
                   ((r >= 0) && (r < B.size) && (c >= 0) && (c < B.size)))
               () ps);
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* [all_moves] and [is_valid_move] are consistent with each other *)
    "valid_moves" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing
        do let am = B.all_moves !b in
          ignore (List.fold_left
                    (fun _ (ss,es) ->
                       ignore (List.fold_left
                                 (fun _ es' ->
                                    assert_equal true
                                      (B.is_valid_move !b ss es'))
                                 () es))
                    () am);
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* Examines the properties of the record returned by [last_move] *)
    "last_move" >:: (fun _ ->
        let (s1,s2) = A.choose_move brd in
        let b = ref (B.move brd s1 s2) in
        while B.status_of_game !b = Ongoing
        do let lm = B.last_move !b in
          assert_equal false (List.mem lm.start_space B.lakes);
          assert_equal false (List.mem lm.end_space B.lakes);
          begin match (lm.defender,lm.outcome) with
          | (Some _,Some _) -> ()
          | (None,None) -> ()
          | _ -> failwith "Inconsistent Last Move Info"
          end;
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* The number of pieces that have been captured so far never decreases;
     * it only increases as the game goes on *)
    "captured_sum" >:: (fun _ ->
        let num_captured b c =
          List.fold_left (fun x (_,y) -> x + y)
            0 (B.captured_pieces b c) in
        let b = ref brd in
        let cr = ref (num_captured !b Red) in
        let cb = ref (num_captured !b Blue) in
        while B.status_of_game !b = Ongoing do
          let cr' = num_captured !b Red in
          let cb' = num_captured !b Blue in
          assert_equal true (cr' >= !cr);
          assert_equal true (cb' >= !cb);
          cr := cr';
          cb := cb';
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

    (* Every piece with which the game starts is represented either as an
     * active piece or as an entry in a graveyard at all times *)
    "total_piece_count" >:: (fun _ ->
        let b = ref brd in
        while B.status_of_game !b = Ongoing do
          check_piece_count !b Red;
          check_piece_count !b Blue;
          let (s1,s2) = A.choose_move !b in
          b := B.move !b s1 s2
        done);

  ]

end

module GTests8 = GeneralTests (Board8)

module GTests10 = GeneralTests (Board10)

let all_tests = GTests8.tests @ GTests10.tests

let suite = "Stratego Test Suite" >::: all_tests

let _ = run_test_tt_main suite
