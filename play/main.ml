open! Core
open Async
open Game_strategies_common_lib

let is_winning_move ~(game : Game.t) ~(position : Position.t) =
  let direction_pairs =
    let ( >> ) = Fn.compose in
    Position.
      [
        (down, up);
        (right, left);
        (down >> right, up >> left);
        (down >> left, up >> right);
      ]
  in
  let piece = Map.find_exn game.board position in
  List.exists direction_pairs ~f:(fun (offset_1, offset_2) ->
      let rec count_dir pos offset count =
        match Map.find game.board pos with
        | Some p when Piece.equal p piece ->
            count_dir (offset pos) offset (count + 1)
        | _ -> count
      in
      let dir_1 = count_dir (offset_1 position) offset_1 0 in
      let dir_2 = count_dir (offset_2 position) offset_2 0 in
      dir_1 + dir_2 + 1 >= Game_kind.win_length game.game_kind)

let place_piece ~(game_state : State.Game_state.t) ~position =
  let { State.Game_state.game; next_piece; x_name; o_name; _ } = game_state in
  match Position.in_bounds position ~game_kind:game.game_kind with
  | false ->
      State.Game_over
        {
          reason = Illegal_move { error = Out_of_bounds; piece = next_piece };
          game;
          x_name;
          o_name;
        }
  | true -> (
      match Map.find game.board position with
      | Some piece ->
          Game_over
            {
              reason =
                Illegal_move
                  { error = Occupied_space piece; piece = next_piece };
              game;
              x_name;
              o_name;
            }
      | None -> (
          let game = Game.set_piece game position next_piece in
          match is_winning_move ~game ~position with
          | true ->
              Game_over { reason = Winner next_piece; game; x_name; o_name }
          | false ->
              let size = Game_kind.board_length game.game_kind in
              if size * size = Map.length game.board then
                Game_over { reason = Draw; game; x_name; o_name }
              else
                Game_in_progress
                  { game_state with game; next_piece = Piece.flip next_piece }))

let rec play game_state =
  Display.render (Game_in_progress game_state);
  let%bind next_state =
    let you_play, connection =
      match game_state with
      | { next_piece = X; x_connection; _ } -> (Piece.X, x_connection)
      | { next_piece = O; o_connection; _ } -> (O, o_connection)
    in
    match%map
      Clock_ns.with_timeout game_state.move_timeout
        (Rpc.Rpc.dispatch Rpcs.Take_turn.rpc connection
           { game = game_state.game; you_play })
    with
    | `Result (Ok position) -> place_piece ~game_state ~position
    | `Result (Error error) ->
        print_s
          [%message
            "Error requesting move" (error : Error.t) (you_play : Piece.t)];
        State.Game_over
          {
            reason = Error_requesting_move { piece = you_play; error };
            game = game_state.game;
            x_name = game_state.x_name;
            o_name = game_state.o_name;
          }
    | `Timeout ->
        State.Game_over
          {
            reason = Player_timed_out you_play;
            game = game_state.game;
            x_name = game_state.x_name;
            o_name = game_state.o_name;
          }
  in
  match next_state with
  | Game_in_progress next_game_state -> play next_game_state
  | Game_over { reason; _ } ->
      Display.render next_state;
      return reason

let start_server ~port =
  let%bind server =
    Rpc.Connection.serve
      ~implementations:
        (Rpc.Implementations.create_exn ~on_unknown_rpc:`Close_connection
           ~implementations:
             [
               Rpc.Rpc.implement Rpcs.Take_turn.rpc (fun _ { game; you_play } ->
                   return (Game_strategies_lib.Main.make_move ~game ~you_play));
             ])
      ~initial_connection_state:(fun _client_identity _client_addr -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  Tcp.Server.close_finished server

let wait_for_game_command =
  Command.async ~summary:"Wait for a game to start"
    (let%map_open.Command () = return ()
     and port = flag "port" (required int) ~doc:"INT server port" in
     fun () -> start_server ~port)

let start_game_command =
  Command.async ~summary:"Start a game"
    (let%map_open.Command () = return ()
     and x_host_and_port =
       flag "x" (required host_and_port) ~doc:"_ <host>:<port> of X player"
     and o_host_and_port =
       flag "o" (required host_and_port) ~doc:"_ <host>:<port> of O player"
     and x_name = flag "x-name" (required string) ~doc:"STRING X player name"
     and o_name = flag "o-name" (required string) ~doc:"STRING O player name"
     and omok = flag "omok" no_arg ~doc:"_ play Omok instead of Tic Tac Toe"
     and move_timeout =
       flag_optional_with_default_doc "move-timeout" Time_ns.Span.arg_type
         [%sexp_of: Time_ns.Span.t] ~default:(Time_ns.Span.of_sec 30.)
         ~doc:"SPAN time allowed for each move (e.g. 10s, 1m30s, etc.)"
     in
     fun () ->
       let connect host_and_port =
         Rpc.Connection.client
           (Tcp.Where_to_connect.of_host_and_port host_and_port)
         >>| Result.ok_exn
       in
       let%bind x_connection = connect x_host_and_port
       and o_connection = connect o_host_and_port in
       let game_state =
         {
           State.Game_state.game =
             Game.empty (if omok then Omok else Tic_tac_toe);
           next_piece = List.random_element_exn [ Piece.X; O ];
           x_connection;
           o_connection;
           x_name;
           o_name;
           move_timeout;
         }
       in
       Display.init game_state.game.game_kind;
       let%bind (_ : State.Game_over_reason.t) = play game_state in
       Deferred.never ())

let command =
  Command.group ~summary:"Game Strategies"
    [
      ("wait-for-game", wait_for_game_command);
      ("start-game", start_game_command);
    ]
