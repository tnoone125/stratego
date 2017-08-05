open Game
open Main

 (* First string printed out to the user. *)
let opener = "Welcome to Stratego. Press 1 to begin a 1 player Game,
press 2 to begin a 2 player game."

(* The empty board that is given to the state. *)
let b = [|
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; Some {piece=Lake; seen=true; owner=0; color=0}; Some {piece=Lake; seen=true; owner=0; color=0}; 
    None; None; Some {piece=Lake; seen=true; owner=0; color=0}; Some {piece=Lake; seen=true; owner=0; color=0}; None; None|];
  [| None; None; Some {piece=Lake; seen=true; owner=0; color=0}; Some {piece=Lake; seen=true; owner=0; color=0}; 
    None; None; Some {piece=Lake; seen=true; owner=0; color=0}; Some {piece=Lake; seen=true; owner=0; color=0}; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
  [| None; None; None; None; None; None; None; None; None; None|];
|]

(* The initial state fed to the first repl in main.ml *)
let init_state = {board=b; turns=0; player_to_move=1; mode=Two_Player; to_print = opener; quit=false; winner = 0;}

(* This calls on the first REPL in main as long as the user has not quit and no one has won yet. *)
let _ = while (init_state.quit=false)&&(init_state.winner = 0) do (Main.repl1 init_state) done
