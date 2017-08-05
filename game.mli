(* The game mode can either be one player (plays against the AI) or
 * two player. *)
type mode = One_Player | Two_Player

(* These are the kinds of tokens in Stratego. All can move except Bombs, 
 * Flags, and Lakes *)
type piece = Bomb | Spy | Nine | Eight | Seven | Six | Five | Four | Three 
  | Two | One | Flag | Lake

(* This contains more information about the piece: the kind of piece, whether
 * or not it has been seen by the opponent, and the owner. *)
type full_piece = {
	piece: piece;
	mutable seen: bool;
	owner: int;
  mutable color: int;
}

(* These are all valid directions in the game. *)
type direction = Up | Down | Left | Right

type result = Capture | Fail | Draw | Nil

(* A board is a 2D array, and each position in the array refers to a full_piece
 * option. We chose to use an array rather than lists because it is easy to
 * access the contents. *)
type board = full_piece option array array

(* The game state contains information about the overall game, mainly the game
 * mode, number of turns that have taken place, the player whose turn it is,
 * and the current board. *)
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
val string_of_piece : piece -> string

(* [battle att def] is the result of a battle between two particular
 * pieces according to the rules of Stratego, which can be Capture, Draw, Fail, 
 * or Nil. [battle] is nil only if def is Lake, or if att is Flag, Bomb, 
 * or Lake. When att and def are the same piece, [battle] is Draw. *)
val battle : piece -> piece -> result

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
val advance : state -> int*int -> direction -> unit

(* [p_alive pl st] is true iff the player pl has not yet lost the game in
 * st. The player still has moves to make if his/her flag is still
 * on the board and if the player owns at least one movable piece (anything
 * besides a Lake, Flag, or Bomb)
 *)
val p_alive : int -> state -> bool
