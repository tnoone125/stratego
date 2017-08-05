open Game

(* setup_random st returns st with the AI's pieces randomly placed within the
 * AI's side of the board *)
val setup_random : state -> unit


(* advance_ai st chooses a move for the AI based on choose_move and advances
 * the game and state by moving the AI in the direction *)
val advance_ai : state -> unit

(* choose_move st chooses a move that the current player can play in state st
 * using the ai's strategy *)
val choose_move : state -> (int*int)*direction
