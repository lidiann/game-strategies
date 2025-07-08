open! Core
open! Async

type t = { game_kind : Game_kind.t; board : Piece.t Position.Map.t }
[@@deriving sexp_of, bin_io]

let empty game_kind = { game_kind; board = Position.Map.empty }

let set_piece t position piece =
  { t with board = Map.set t.board ~key:position ~data:piece }

let get_2D_piece_list (game : t) =
  let board_length = Game_kind.board_length game.game_kind in
  let board = game.board in
  List.init board_length ~f:(fun row ->
      List.init board_length ~f:(fun column -> Map.find board { row; column }))
