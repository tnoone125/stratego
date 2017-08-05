open Game
open Ai

(* This is the REPL with which the user interacts. Each turn the user
 * sees the updated game state. 
 * [repl1] is the first repl called, in runner.ml, which begins the game
 * and introduces the user to the game. *)
val repl1 : state -> unit
