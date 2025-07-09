open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let print_game (game : Game.t) =
  let board_length = Game_kind.board_length game.game_kind in
  let pieces_list = Game.get_2D_piece_list game in
  List.iteri pieces_list ~f:(fun row col_list ->
      List.iteri col_list ~f:(fun column piece ->
          match piece with
          | None ->
              print_string "  ";
              if not (Int.equal column (board_length - 1)) then
                print_string "| "
          | Some piece ->
              print_string (Piece.to_string piece ^ " ");
              if not (Int.equal column (board_length - 1)) then
                print_string "| ");
      print_string "\n";
      if not (Int.equal row (board_length - 1)) then
        print_endline (Game_kind.row_separator game.game_kind))

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  (* let board_length = Game_kind.board_length game.game_kind in *)
  let pieces_list = Game.get_2D_piece_list game in
  (* Essentially like Map.find on board. *)
  List.concat
    (List.mapi pieces_list ~f:(fun row col_list ->
         List.filter_mapi col_list ~f:(fun column piece ->
             match piece with
             (* Should match on Piece variant after fixes *)
             | None -> (Some { row; column } : Position.t option)
             | Some X | Some O -> None)))

let%expect_test "available_moves_win_for_x" =
  let test_available_moves = available_moves win_for_x in
  print_endline
    (List.to_string
       ~f:(fun position -> Position.to_string position)
       test_available_moves);
  [%expect {|
  ()
  |}];
  return ()

let%expect_test "available_moves_non_win" =
  let test_available_moves = available_moves non_win in
  print_endline
    (List.to_string
       ~f:(fun position -> Position.to_string position)
       test_available_moves);
  [%expect
    {|
    ("((row 0) (column 1))""((row 0) (column 2))""((row 1) (column 1))""((row 1) (column 2))""((row 2) (column 1))")
  |}];
  return ()

let get_next_piece (game : Game.t) direction position =
  let win_length = Game_kind.win_length game.game_kind in
  List.init win_length ~f:(fun until_win ->
      let rec move_in dir posn until_win =
        if Int.equal until_win 0 then posn
        else move_in dir (dir posn) (until_win - 1)
      in
      let new_posn = move_in direction position until_win in
      Map.find game.board new_posn)

let check_directions (game : Game.t) position (cur_piece : Piece.t) :
    Evaluation.t =
  List.fold Position.some_offsets
    ~init:(Game_continues : Evaluation.t)
    ~f:(fun eval direction ->
      match eval with
      | Illegal_move -> Illegal_move
      | Game_over piece -> Game_over piece
      | Game_continues -> (
          match Position.in_bounds position ~game_kind:game.game_kind with
          | false -> Illegal_move
          | true -> (
              let next_pieces = get_next_piece game direction position in
              match
                List.for_all next_pieces ~f:(fun next_piece ->
                    match next_piece with
                    | None -> false
                    | Some nxt_piece -> Piece.equal nxt_piece cur_piece)
              with
              | true -> Game_over { winner = Some cur_piece }
              | false -> Game_continues)))

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let result =
    Map.fold game.board
      ~init:(Game_continues : Evaluation.t)
      ~f:(fun ~key:position ~data:piece game_eval ->
        match game_eval with
        | Illegal_move -> Illegal_move
        | Game_over piece -> Game_over piece
        | Game_continues -> check_directions game position piece)
  in

  match
    Int.equal (Map.length game.board) (Game_kind.board_size game.game_kind)
  with
  | true -> (
      match result with
      | Game_continues -> Game_over { winner = None }
      | Illegal_move | Game_over _ -> result)
  | false -> result

let%expect_test "evaluate_non_win" =
  let is_over = evaluate non_win in
  print_s (Evaluation.sexp_of_t is_over);
  [%expect {|
    Game_continues
  |}];
  return ()

let%expect_test "evaluate_win_for_x" =
  let is_over = evaluate win_for_x in
  print_s (Evaluation.sexp_of_t is_over);
  [%expect {|
    (Game_over (winner (X)))
  |}];
  return ()

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  (* For moves in available_moves, if added to game.board they give Game_over Some me, add to list. So, filter available moves for if they give Game_over {winner=Some me} *)
  let available_moves = available_moves game in
  List.filter available_moves ~f:(fun position ->
      let potential_board = Map.add_exn game.board ~key:position ~data:me in
      match
        evaluate { game_kind = game.game_kind; board = potential_board }
      with
      | Illegal_move | Game_continues -> false
      | Game_over { winner = piece } -> (
          match piece with None -> false | Some piece -> Piece.equal me piece))

let%expect_test "moves_win_for_x" =
  let winning_moves = winning_moves ~me:X win_for_x in
  List.iter winning_moves ~f:(fun position ->
      print_endline (Position.to_string position));
  [%expect {|
  |}];
  return ()

let%expect_test "moves_non_win" =
  let winning_moves = winning_moves ~me:X non_win in
  List.iter winning_moves ~f:(fun position ->
      print_endline (Position.to_string position));
  [%expect {|
  ((row 1) (column 1))
  |}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let my_available_moves = available_moves game in
  List.filter my_available_moves ~f:(fun position ->
      let potential_board = Map.add_exn game.board ~key:position ~data:me in
      let opps_winning_moves =
        winning_moves ~me:(Piece.flip me)
          { game_kind = game.game_kind; board = potential_board }
      in
      not (List.is_empty opps_winning_moves))

let%expect_test "losing_moves_non_win" =
  let losing_moves = losing_moves ~me:X non_win in
  List.iter losing_moves ~f:(fun position ->
      print_endline (Position.to_string position));
  [%expect {|
  |}];
  return ()

let%expect_test "losing_moves_win_for_x" =
  let losing_moves = losing_moves ~me:X win_for_x in
  List.iter losing_moves ~f:(fun position ->
      print_endline (Position.to_string position));
  [%expect {|
  |}];
  return ()

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  match winning_moves ~me:you_play game with
  | position :: _ -> position
  | [] -> (
      let potential_good_moves =
        List.filter (available_moves game) ~f:(fun position ->
            not
              (List.mem
                 (losing_moves ~me:you_play game)
                 position ~equal:Position.equal))
      in
      match potential_good_moves with
      | position :: _ -> position
      | [] -> List.random_element_exn (available_moves game))
