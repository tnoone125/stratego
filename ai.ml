open Game
exception Failure1

let b = [|
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; Some {piece=Lake; seen=true; owner=0; color=0};
    Some {piece=Lake; seen=true; owner=0; color=0};
    None; None; Some {piece=Lake; seen=true; owner=0; color=0};
    Some {piece=Lake; seen=true; owner=0; color=0}; None; None|];
  [| None; None; Some {piece=Lake; seen=true; owner=0; color=0};
    Some {piece=Lake; seen=true; owner=0; color=0};
    None; None; Some {piece=Lake; seen=true; owner=0; color=0};
    Some {piece=Lake; seen=true; owner=0; color=0}; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|]; |]

let piecelist = [Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Bomb; seen=false; owner=2; color=2};
                 Some {piece=Flag; seen=false; owner=2; color=2};
                 Some {piece=Spy; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Nine; seen=false; owner=2; color=2};
                 Some {piece=Eight; seen=false; owner=2; color=2};
                 Some {piece=Eight; seen=false; owner=2; color=2};
                 Some {piece=Eight; seen=false; owner=2; color=2};
                 Some {piece=Eight; seen=false; owner=2; color=2};
                 Some {piece=Eight; seen=false; owner=2; color=2};
                 Some {piece=Seven; seen=false; owner=2; color=2};
                 Some {piece=Seven; seen=false; owner=2; color=2};
                 Some {piece=Seven; seen=false; owner=2; color=2};
                 Some {piece=Seven; seen=false; owner=2; color=2};
                 Some {piece=Six; seen=false; owner=2; color=2};
                 Some {piece=Six; seen=false; owner=2; color=2};
                 Some {piece=Six; seen=false; owner=2; color=2};
                 Some {piece=Six; seen=false; owner=2; color=2};
                 Some {piece=Five; seen=false; owner=2; color=2};
                 Some {piece=Five; seen=false; owner=2; color=2};
                 Some {piece=Five; seen=false; owner=2; color=2};
                 Some {piece=Five; seen=false; owner=2; color=2};
                 Some {piece=Four; seen=false; owner=2; color=2};
                 Some {piece=Four; seen=false; owner=2; color=2};
                 Some {piece=Four; seen=false; owner=2; color=2};
                 Some {piece=Three; seen=false; owner=2; color=2};
                 Some {piece=Three; seen=false; owner=2; color=2};
                 Some {piece=Two; seen=false; owner=2; color=2};
                 Some {piece=One; seen=false; owner=2; color=2};]

let board_pos = [(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);(0,9);
                (1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7);(1,8);(1,9);
                (2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7);(2,8);(2,9);
                (3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7);(3,8);(3,9);
                (4,0);(4,1);(4,4);(4,5);(4,8);(4,9);
                (5,0);(5,1);(5,4);(5,5);(5,8);(5,9);
                (6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7);(6,8);(6,9);
                (7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7);(7,8);(7,9);
                (8,0);(8,1);(8,2);(8,3);(8,4);(8,5);(8,6);(8,7);(8,8);(8,9);
                (9,0);(9,1);(9,2);(9,3);(9,4);(9,5);(9,6);(9,7);(9,9);(9,9);]


(* [valid_piece y x st] returns true if the piece occupying spot y x in state
 * st is a movable piece owned by the current player *)
let valid_piece y x st = match st.board.(y).(x) with
  | None -> false
  | Some pc -> if pc.owner<>st.player_to_move
    || pc.piece=Flag || pc.piece=Bomb || pc.piece=Lake then false else true

(* [deep_copy st] returns a copy of st which when modified, does not affect st*)
let copy_board board = Array.map Array.copy board
let deep_copy st =
  {turns=st.turns;
  player_to_move = st.player_to_move;
  mode = st.mode;
  to_print = st.to_print;
  board = copy_board st.board;
  quit = st.quit;
  winner = st.winner;}

(* [shuffle_list d] returns the list d that has been shuffled randomly *)
let shuffle_list d = Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

(* [setup_random st] modifies st to place all of the AI's pieces *)
let setup_random st =
  let x = ref 0 in let y = ref 0 in
  let _ = List.map (fun current_piece -> if (!y < 10) then
  begin st.board.(!x).(!y) <- current_piece; y := !y + 1 end
  else begin (x:=!x+1); (y := 0);  st.board.(!x).(!y) <- current_piece;
  y := !y+1 end) (shuffle_list piecelist) in ()

(* [attack_valid y x st flpiece] returns true if the AI can attack the piece at
 * y x in state st with piece flpiece without being guaranteed to lose *)
let attack_valid y x st flpiece =
  if y<0 || y>9 || x<0 || x>9 then false else match st.board.(y).(x) with
  | None -> false
  | Some pc -> if (pc.owner<>st.player_to_move && pc.piece<>Lake) then if
    pc.seen=false then true else if (Game.battle flpiece pc.piece =Capture) then
    true else false else false

(* [attack_valid_wrap y x st dir] returns true if the AI can attack in the
 * direction dir with its piece at y x in state st without being guaranteed
 * to lose *)
let attack_valid_wrap y x st dir = match st.board.(y).(x) with
  | None -> false
  | Some pc -> if (not (valid_piece y x st)) then false else begin
    match dir with
      | Up -> (attack_valid (y-1) x st pc.piece)
      | Down -> (attack_valid (y+1) x st pc.piece)
      | Left -> (attack_valid y (x-1) st pc.piece)
      | Right -> (attack_valid y (x+1) st pc.piece)
  end

(* [move_valid y x st] returns true if the space y x is occupied by either the
 * other player's piece or nothing *)
let move_valid y x st = if y<0 || y>9 || x<0 || x>9 then false else
  match st.board.(y).(x) with
    | None -> true
    | Some pc -> if (pc.owner<>st.player_to_move && pc.piece<>Lake)
      then true else false

(* [move_valid_wrap y x st dir] returns true if the AI can move its piece at
 * y x in state st in direction dir without violating any rules *)
let move_valid_wrap y x st dir = match st.board.(y).(x) with
  | None -> false
  | Some pc -> if (not (valid_piece y x st)) then false else
    begin match dir with
    | Up -> move_valid (y-1) x st
    | Down -> move_valid (y+1) x st
    | Left -> move_valid y (x-1) st
    | Right -> move_valid y (x+1) st
    end

(* [possible_movesyx y x st] returns a list of directions that the AI can move
 * its piece at y x in state st *)
let possible_movesyx y x st =
  List.filter (move_valid_wrap y x st) [Up;Down;Left;Right]

(* dangerous_piece returns true if there exists an opponent's piece at y x in
 * state st that can defeat piece piec *)
let dangerous_piece y x st piec = if y<0 || y>9 || x<0 || x>9 then false else
  match st.board.(y).(x) with
  | None -> false
  | Some pc -> if pc.owner<>st.player_to_move && (battle pc.piece piec)=Capture
    then true else false

(* dangerous_spot y x st piec returns true if there is a dangerous piece
 * adjacent to y x that could defeat piece piec*)
let dangerous_spot y x st piec = (dangerous_piece (y-1) x st piec) ||
  (dangerous_piece (y+1) x st piec) || (dangerous_piece y (x-1) st piec) ||
  (dangerous_piece y (x+1) st piec)


(* dangerous_move y x st piec dir returns true if moving piece piec in the
 * direction dir from y x in state st puts the piece in a dangerous spot *)
let dangerous_move y x st piec dir = match dir with
  | Up -> dangerous_spot (y-1) x st piec
  | Down -> dangerous_spot (y+1) x st piec
  | Left -> dangerous_spot y (x-1) st piec
  | Right -> dangerous_spot y (x+1) st piec


let moveable_location y x st = if y<0 || y>9 || x<0 ||x>9 then false else
  match st.board.(y).(x) with
  | None -> true
  | Some pc -> if pc.piece=Lake || pc.owner=st.player_to_move
    then false else true

(* moveable_direction y x st dir returns true if in state st, there exists a
 * player's piece at position y x that can move in direction dir *)
let moveable_direction y x st dir = match dir with
  | Up -> moveable_location (y-1) x st
  | Down -> moveable_location (y+1) x st
  | Left -> moveable_location y (x-1) st
  | Right -> moveable_location y (x+1) st

(* attack_possible returns true if the current player to move can move move his
 * piece at y x in direction dir to attack the opponent's piece *)
let attack_possible y x st dir = match st.board.(y).(x) with
  | None -> false
  | Some pc -> if pc.owner=st.player_to_move then
    moveable_direction y x st dir else false

(* safe_attack returns true if the piece can in position y x can attack in the
 * direction dir without being guaranteed to lose and be put in a vulnerable
 * spot where it can be defeated, false otherwise *)
let safe_attack y x st dir = match st.board.(y).(x) with
  | None -> false
  | Some pc ->  if (not (attack_possible y x st dir)) then false else
    (attack_valid_wrap y x st dir) && (not (dangerous_move y x st pc.piece dir))

(* [possible_moves st] returns the (y,x) location of the pieces that the current
 * player owns and can make a valid move on *)

let player_piece_locs st =
  List.filter (fun (y,x) -> match st.board.(y).(x) with
  None -> false | Some pc -> if pc.owner=st.player_to_move then true else false)
  board_pos


(* possible_moves st returns the list of possible locations and moves that the
 * current player to move's pieces can make*)
let possible_moves st =
  List.fold_left (fun acc1 (y,x) -> (List.fold_left
    (fun acc elem -> if (move_valid_wrap y x st elem) then
    ((y,x),elem)::acc else acc) [] [Up;Down;Left;Right])@acc1)
    [] (player_piece_locs st)

(* dumb_move_lst st mov_piece returns the list of the locations of pieces and
 * directions in which those pieces can be moved in a valid move *)
let dumb_move_lst st mov_pieces = match  (List.filter (fun ((y,x),dir) ->
  (attack_valid_wrap y x st dir) ) mov_pieces) with
    | [] ->
      (List.filter (fun ((y,x),dir) -> (move_valid_wrap y x st dir)) mov_pieces)
    | lst -> lst


(* dumb_move_flag_area st mov_pieces takes in the state and the player's movable
 * pieces and chooses one that moves a piece toward the area where the opponents
 * flag could be located *)
let dumb_move_flag_area st mov_pieces =
  let dumb_lst = (dumb_move_lst st mov_pieces) in
  (List.filter (fun ((y,x),dir) -> if st.player_to_move=1
    then (dir=Up || y<4) else (dir=Down || y>5)) dumb_lst)


(* choose_move st chooses a move that the current player can play in state st
 * using the ai's strategy *)

let choose_move st = let mov_pieces = (possible_moves st) in begin
  match (List.filter (fun ((y,x),dir) -> (safe_attack y x st dir)) mov_pieces)
  with  | [] -> begin match (dumb_move_flag_area st mov_pieces) with
        | [] -> begin match (dumb_move_lst st mov_pieces) with
        | [] -> raise Failure1
        | lst -> (List.hd (shuffle_list lst)) end
     | lst -> (List.hd (shuffle_list lst)) end
   | lst -> (List.hd (shuffle_list lst))
  end


(* advance_ai st chooses a move for the AI based on choose_move and advances
 * the game and state by moving the AI in the direction *)
let advance_ai st =
  let ((y,x),dir) = choose_move st in Game.advance st (y,x) dir
