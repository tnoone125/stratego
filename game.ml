exception Invalid

(* Here are various strings that may be placed into st.to_print *)
let cap = "Piece captured!"
let fail = "Capture failed!"
let draw = "It's a draw! Both pieces eliminated!"
let advance = "Piece advances!"
let invalid = "Not a valid move, try again."
let not_piece = "That's not your piece, try again."
let bomb = "You cannot move bombs."
let flag = "You cannot move flags."

(* Here are the two modes available. In one player mode, the user plays
 * against the AI. *)
type mode = One_Player | Two_Player

(* These are the kinds of tokens in Stratego. All can move except Bombs,
 * Flags, and Lakes *)
type piece = Bomb | Spy | Nine | Eight | Seven | Six | Five | Four | Three
  | Two | One | Flag | Lake

(* type full_piece contains more information about the piece: the kind of piece,
* whether or not it has been seen by the opponent, the owner, 
* and color as an int for the UI. *)
type full_piece = {
  piece: piece;
  mutable seen: bool;
  owner: int;
  mutable color: int;
}

(* Here are the four directions a piece can move. *)
type direction = Up | Down | Left | Right

(* type result is one of the four results of a battle. In Capture, the
 * attacking piece eliminates the defending piece and takes its spot.
 * If it fails to capture, it is eliminated from the board. In a draw,
 * both are eliminated from the board. Nil represents the case where a piece
 * tries to attack a Lake, which is not valid. *)
type result = Capture | Fail | Draw | Nil

(* The board is a full_piece option array array, or a 2D matrix. Each
 * position in the matrix either contains none or Some full_piece. *)
type board = full_piece option array array

(* The type state contains all relevant information for a particular game.
 * All of its fields are mutable so that it can reflect any particular game
 * after initializing as init_state in runner.ml. Specifically, to_print
 * will change at every call of the REPL, mode is set at the beginning of
 * the game, winner changes when a player has won, lost or drew, and
 * quit becomes true if a user at any point enters "QUIT". *)
type state = {mutable board: board;
  mutable turns: int;
  mutable player_to_move: int;
  mutable mode: mode;
  mutable to_print : string;
  mutable quit : bool;
  mutable winner : int;
  }

(* [string_of_piece p] is a string representation of some piece. The string
 * contains the Stratego "military" name for the piece and its corresponding
 * number.
 * precondition: p is a valid piece. *)
let string_of_piece = function
  | Spy -> "Spy (S)"
  | Nine -> "Scout (9)"
  | Eight -> "Miner (8)"
  | Seven -> "Sergeant (7)"
  | Six -> "Lieutenant (6)"
  | Five -> "Captain (5)"
  | Four -> "Major (4)"
  | Three -> "Colonel (3)"
  | Two -> "General (2)"
  | One -> "Marshal (1)"
  | Flag -> "Flag (F)"
  | Bomb -> "Bomb (B)"
  | Lake -> "Lake (L)"

(* [change_move st] is a method that mutates the state's player_to_move
 * either from 1 to 2 or 2 to 1.
 * precondition: st.player_to_move is either 1 or 2. *)
let change_move st = if st.player_to_move = 1 then st.player_to_move <- 2
  else st.player_to_move <- 1

(* [battle att def] is the result of a battle between two particular
 * pieces according to the rules of Stratego, which can be Capture, Draw, Fail,
 * or Nil. [battle] is nil only if def is Lake, or if att is Flag, Bomb,
 * or Lake. When att and def are the same piece, [battle] is Draw. *)
let battle (att: piece) (def: piece) =
  match att with
  | Spy -> begin
    match def with
    | One | Flag -> Capture
    | Spy -> Draw
    | Lake -> Nil
    | _ -> Fail
  end
  | Nine -> begin
    match def with
    | Spy | Flag -> Capture
    | Nine -> Draw
    | Lake -> Nil
    | _ -> Fail
  end
  | Eight -> begin
    match def with
    | Spy | Nine | Bomb | Flag -> Capture
    | Eight -> Draw
    | Lake -> Nil
    | _ -> Fail
  end
  | Seven -> begin
    match def with
    | Spy | Nine | Eight | Flag -> Capture
    | Seven -> Draw
    | Lake -> Nil
    | _ -> Fail
  end
  | Six -> begin
    match def with
    | Spy | Nine | Eight | Seven | Flag -> Capture
    | Six -> Draw
    | Lake -> Nil
    | _ -> Fail
  end
  | Five -> begin
    match def with
    | Bomb | One | Two | Three | Four -> Fail
    | Lake -> Nil
    | Five -> Draw
    | _ -> Capture
  end
  | Four -> begin
    match def with
    | Bomb | One | Two | Three -> Fail
    | Lake -> Nil
    | Four -> Draw
    | _ -> Capture
  end
  | Three -> begin
    match def with
    | Bomb | One | Two -> Fail
    | Lake -> Nil
    | Three -> Draw
    | _ -> Capture
  end
  | Two -> begin
    match def with
    | Bomb | One -> Fail
    | Lake -> Nil
    | Two -> Draw
    | _ -> Capture
  end
  | One -> begin
    match def with
    | Bomb -> Fail
    | Lake -> Nil
    | One -> Draw
    | _ -> Capture
  end
  | Bomb -> Nil
  | Flag -> Nil
  | Lake -> Nil

(* [advance st (y,x) dir] is a method that updates the game state
 * after processing a move: the piece located at (y,x) moving one space
 * in the direction dir. If there does not exist a piece at (y,x), the board
 * does not change and to_print is given a string that says the move is invalid.
 * Similarly, if the user attempts to move a piece he/she does not own, or
 * if he/she tries to move a Lake piece, the board will not change and the
 * user will be told that the move is invalid. If a valid move is to a position
 * where there is no piece, the piece simply advances to that position.
 * If it attempts to move to a position with an enemy piece, the two pieces
 * battle and the board updates according to the result of the battle.
 * A piece cannot battle one that has the same owner. In all moves,
 * st.to_print is mutated to output the results of the move.
 *
 * precondition: st is a valid game state, (y,x) is a valid position in st.board,
 * and dir is a valid direction (Up, Down, Left, or Right)
 * postcondition: the mutated st will be a valid game state.*)
let advance st (y,x) dir =
  match st.board.(y).(x) with
  | None -> st.to_print <- invalid; ()
  | Some f1 -> if f1.owner <> st.player_to_move then begin 
      st.to_print <- not_piece; () end else
    if f1.piece = Bomb then begin st.to_print <- bomb; () end else 
    if f1.piece = Flag
      then begin st.to_print <- flag; () end else
    begin
      match dir with
      | Up -> let opp = st.board.(y-1).(x) in begin
        match opp with
        | None -> st.board.(y-1).(x) <-st.board.(y).(x); st.board.(y).(x)<-None;
          st.to_print <- "Player "^(string_of_int (st.player_to_move))^
            "'s piece at ("^(string_of_int y)^", "^(string_of_int x)^
            ") advanced to ("^(string_of_int (y-1))^", "^(string_of_int x)^")!"; 
          change_move st; ()
        | Some f2 -> if f1.owner = f2.owner then begin st.to_print <- invalid; 
          () end else begin
          match battle f1.piece f2.piece with
          | Capture -> st.board.(y-1).(x) <- st.board.(y).(x); st.to_print <-
            ("The "^(string_of_piece f1.piece)^" captured the "^
              (string_of_piece f2.piece)^"!");
            st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; ()
          | Fail -> st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; st.to_print <-
            ("The "^(string_of_piece f1.piece)^" failed to capture the "^
              (string_of_piece f2.piece)^"!"); ()
          | Draw -> st.board.(y).(x) <- None; st.board.(y-1).(x) <- None; 
            f1.seen <- true; f2.seen <- true; st.to_print <- 
            ("The two "^(string_of_piece f1.piece)^"s killed each other!"); 
            change_move st; ()
          | Nil -> st.to_print <- invalid; ()
        end
      end
      | Down -> let opp = st.board.(y+1).(x) in begin
        match opp with
        | None -> st.board.(y+1).(x) <-st.board.(y).(x); st.board.(y).(x)<-None;
          st.to_print <- "Player "^(string_of_int (st.player_to_move))^
            "'s piece at ("^(string_of_int y)^", "^(string_of_int x)^
            ") advanced to ("^(string_of_int (y+1))^", "^(string_of_int x)^")!"; 
          change_move st; ()
        | Some f2 -> if f1.owner = f2.owner then begin st.to_print <- invalid; 
          () end else begin
          match battle f1.piece f2.piece with
          | Capture -> st.board.(y+1).(x) <- st.board.(y).(x); st.to_print <-
            ("The "^(string_of_piece f1.piece)^" captured the "^
              (string_of_piece f2.piece)^"!");
            st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; ()
          | Fail -> st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; st.to_print <-
            ("The "^(string_of_piece f1.piece)^" failed to capture the "^
              (string_of_piece f2.piece)^"!"); ()
          | Draw -> st.board.(y).(x) <- None; st.board.(y+1).(x) <- None; 
            f1.seen <- true; f2.seen <- true;
            st.to_print <- ("The two "^(string_of_piece f1.piece)^
              "s killed each other!"); change_move st; ()
          | Nil -> st.to_print <- invalid; ()
        end
      end
      | Left -> let opp = st.board.(y).(x-1) in begin
        match opp with
        | None -> st.board.(y).(x-1) <-st.board.(y).(x); st.board.(y).(x)<-None;
          st.to_print <- "Player "^(string_of_int (st.player_to_move))^
          "'s piece at ("^(string_of_int y)^", "^
            (string_of_int x)^") advanced to ("^(string_of_int (y))^", "^
              (string_of_int (x-1))^")!"; change_move st; ()
        | Some f2 -> if f1.owner = f2.owner then begin st.to_print <- invalid; 
          () end else begin
          match battle f1.piece f2.piece with
          | Capture -> st.board.(y).(x-1) <- st.board.(y).(x); st.to_print <-
            ("The "^(string_of_piece f1.piece)^" captured the "^
              (string_of_piece f2.piece)^"!");
            st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; ()
          | Fail -> st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; st.to_print <-
            ("The "^(string_of_piece f1.piece)^" failed to capture the "^
              (string_of_piece f2.piece)^"!"); ()
          | Draw -> st.board.(y).(x) <- None; st.board.(y).(x-1) <- None; 
            f1.seen <- true; f2.seen <- true;
            st.to_print <- ("The two "^(string_of_piece f1.piece)^
              "s killed each other!"); change_move st; ()
          | Nil -> st.to_print <- invalid; ()
        end
      end
      | Right -> let opp = st.board.(y).(x+1) in begin
        match opp with
        | None -> st.board.(y).(x+1) <-st.board.(y).(x); st.board.(y).(x)<-None;
          st.to_print <- "Player "^(string_of_int (st.player_to_move))^
          "'s piece at ("^(string_of_int y)^", "^(string_of_int x)^
            ") advanced to ("^(string_of_int (y))^", "^
            (string_of_int (x+1))^")!"; change_move st; ()
        | Some f2 -> if f1.owner = f2.owner then begin st.to_print <- invalid; 
          () end else begin
          match battle f1.piece f2.piece with
          | Capture -> st.board.(y).(x+1) <- st.board.(y).(x); st.to_print <-
            ("The "^(string_of_piece f1.piece)^" captured the "^
              (string_of_piece f2.piece)^"!");
            st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; ()
          | Fail -> st.board.(y).(x) <- None; f1.seen <- true; f2.seen <- true; 
            change_move st; st.to_print <-
            ("The "^(string_of_piece f1.piece)^" failed to capture the "^
              (string_of_piece f2.piece)^"!"); ()
          | Draw -> st.board.(y).(x) <- None; st.board.(y).(x+1) <- None; 
            f1.seen <- true; f2.seen <- true;
            st.to_print <- ("The two "^(string_of_piece f1.piece)^
              "s killed each other!"); change_move st; ()
          | Nil -> st.to_print <- invalid; ()
        end
      end
    end

(* [board_to_list st] is a list containing the contents of every position in
 * the 2D array st.board. *)
let board_to_list st =
  let l = ref [] in let _ = for x = 0 to 9 do 
    l := (!l)@(Array.to_list st.board.(x)) done in !l

(* This list represents all of the positions of the board as a simple (y,x) 
 * pair. *)
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

let player_piece_locs st =
  List.filter (fun (y,x) -> match st.board.(y).(x) with
  None -> false | Some pc -> if pc.owner=st.player_to_move then true else false)
  board_pos

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
  | Some pc -> if (not (valid_piece y x st)) then false else begin match dir with
  | Up -> move_valid (y-1) x st
  | Down -> move_valid (y+1) x st
  | Left -> move_valid y (x-1) st
  | Right -> move_valid y (x+1) st
  end

(* [possible_moves st] is the list of possible moves the current
 * st.player_to_move can make. *)
let possible_moves st =
  List.fold_left (fun acc1 (y,x) -> (List.fold_left
  (fun acc elem -> if (move_valid_wrap y x st elem) 
    then ((y,x),elem)::acc else acc)
  [] [Up;Down;Left;Right])@acc1) [] (player_piece_locs st)

(* [p_alive pl st] is true iff the player pl has not yet lost the game in
 * st. The player still has moves to make if his/her flag is still
 * on the board and if the player owns at least one movable piece (anything
 * besides a Lake, Flag, or Bomb)
 *)
let p_alive pl st =
  let l = board_to_list st in
  let rec p_help lst pl acc1 acc2 =
    match lst with
    | [] -> acc1 && acc2
    | h::t -> begin
      match h with
      | None -> p_help t pl acc1 acc2
      | Some f -> if not (f.owner=pl) then p_help t pl acc1 acc2 else 
        if f.piece = Flag then p_help t pl true acc2 else
          if (f.piece=Bomb)||(f.piece=Lake) then p_help t pl acc1 acc2 
            else p_help t pl acc1 true
    end
  and poss_moves pl st = let d=st.player_to_move in let () = st.player_to_move 
    <- pl in let l = possible_moves st in let () = st.player_to_move <- d in
    if l = [] then false else true
in (p_help l pl false false)&&(poss_moves pl st)




