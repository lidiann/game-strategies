open! Core

type t = Tic_tac_toe | Omok [@@deriving sexp_of, equal, bin_io]

let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

let to_string_hum game_kind =
  game_kind |> sexp_of_t |> Sexp.to_string_hum |> String.lowercase
  |> String.map ~f:(function '_' -> ' ' | x -> x)

let board_length = function Tic_tac_toe -> 3 | Omok -> 15
let win_length = function Tic_tac_toe -> 3 | Omok -> 5
let row_separator t = String.init (3 * board_length t) ~f:(fun _ -> '-')
