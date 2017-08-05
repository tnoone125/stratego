open Game
open Ai
open Graphics


(************************************************************************)
(************************************************************************)
(***********************UI/GRAPHICS CODE BELOW***************************)
(************************************************************************)
(************************************************************************)

(*[type click_data] is a record stores the x and y positions of the first
  and second mouse click during gameplay. There is also a message pointer
  field for displaying coordinate of the printed message*)
type click_data = {mutable click1_x: int;
                   mutable click1_y: int;
                   mutable click2_x: int;
                   mutable click2_y: int;
                   mutable msg_pointer: int;
                   mutable player_1: string;}

(*[click_rec] is a record initializing the click coordinates to 0*)
let click_rec = {click1_x = 0; click1_y = 0; click2_x = 0; click2_y = 0;
                 msg_pointer = 0; player_1 = "  ";}

(*[to_colorAA file] takes in a pbm file and converts it to a color array
  array*)
let to_colorAA (file:string) =
  let rec get_dim file_in x =
    (let ch = input_char file_in in
    match ch with
      |'\n' -> int_of_string(x)
      | a   -> get_dim file_in ((x)^(Char.escaped a)))
  in
  let chann = open_in file in
  for k = 0 to 2 do
    ignore(input_char chann);
  done;
  let wid = get_dim chann "" in
  let hgt = get_dim chann "" in
  for k = 0 to 3 do
    ignore(input_char chann);
  done;
  let colorAA = Array.make_matrix hgt wid Graphics.blue in
  for k = 0 to (hgt - 1)
  do
    for j = 0 to (wid - 1)
    do
      let c1 = int_of_char(input_char chann) in
      let c2 = int_of_char(input_char chann) in
      let c3 = int_of_char(input_char chann) in
      let newColor = Graphics.rgb c1 c2 c3 in
      colorAA.(k).(j) <- newColor;
    done;
  done;
  close_in chann; colorAA

(************MODIFIED CODE FROM OCAML GRAPHIC TUTORIAL BELOW******************)
(********URL: https://caml.inria.fr/pub/docs/oreilly-book/html/***************)

(*[type relief] is a record which indicates the 3D orientation of the box*)
type relief = Top | Bot | Flat

(*[type position] is a record which indicates the position within the box*)
type position = Left | Center | Right

(*[type box_config] is a record which defines box configuration*)
type box_config =
{ x:int; y:int; w:int; h:int; bw:int; mutable r:relief;
b1_col : Graphics.color;
b2_col : Graphics.color;
b_col : Graphics.color}

(*[draw_rect x0 y0 w h] takes in the starting x and y coordinates of a box
  and draws a box with width w and height h*)
let draw_rect x0 y0 w h =
let (a,b) = Graphics.current_point()
and x1 = x0+w and y1 = y0+h
in
Graphics.moveto x0 y0;
Graphics.lineto x0 y1; Graphics.lineto x1 y1;
Graphics.lineto x1 y0; Graphics.lineto x0 y0;
Graphics.moveto a b

(*[draw _box_outline bcf col] takes in a box of type box config and outlines it
  in color col*)
let draw_box_outline bcf col =
Graphics.set_color col;
draw_rect bcf.x bcf.y bcf.w bcf.h

(*[draw _box bcf] takes in a box of type box_config and draws it*)
let draw_box bcf =
let x1 = bcf.x and y1 = bcf.y in
let x2 = x1+bcf.w and y2 = y1+bcf.h in
let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw
and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in
let border1 g =
Graphics.set_color g;
Graphics.fill_poly
[| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |]
in
let border2 g =
Graphics.set_color g;
Graphics.fill_poly
[| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
in
Graphics.set_color bcf.b_col;
( match bcf.r with
Top ->
Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
border1 bcf.b1_col;
border2 bcf.b2_col
| Bot ->
Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
border1 bcf.b2_col;
border2 bcf.b1_col
| Flat ->
Graphics.fill_rect x1 y1 bcf.w bcf.h );
draw_box_outline bcf Graphics.black

(*[erase_box bcf] takes in a box of type box_config and erases it from the
  graph*)
let erase_box bcf =
Graphics.set_color bcf.b_col;
Graphics.fill_rect (bcf.x+bcf.bw) (bcf.y+bcf.bw)
(bcf.w-(2*bcf.bw)) (bcf.h-(2*bcf.bw))

(*[draw_string_in_box pos str bcf col] takes in a box bcf and draws a string
  in the specified position within the box in the specified color*)
let draw_string_in_box pos str bcf col =
let (w, h) = Graphics.text_size str in
let ty = bcf.y + (bcf.h-h)/2 in
( match pos with
Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty
| Right -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in
Graphics.moveto tx ty
| Left -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty );
Graphics.set_color col;
Graphics.draw_string str

(*[set_gray x] takes in a value and creates a Graphics color in a gray shade*)
let set_gray x = (Graphics.rgb x x x)

(*[gray1, gray2, gray3] represent three different gray shades*)
let gray1= set_gray 100 and gray2= set_gray 170 and gray3= set_gray 240

(*[create_grid nb_vol n sep b] creates a grid of boxes defined by b with
  n_col columns and n boxes separated by sep distance*)
let rec create_grid nb_col n sep b =
if n < 0 then []
else
let px = n mod nb_col and py = n / nb_col in
let nx = b.x +sep + px*(b.w+sep)
and ny = b.y +sep + py*(b.h+sep) in
let b1 = {b with x=nx; y=ny} in
b1::(create_grid nb_col (n-1) sep b)

(*[vb] creates a 10x10 grid of box type b with 2 pixels separation between
  each box*)
let vb =
let b = {x=0; y=0; w=55;h=55; bw=2;
b1_col=gray1; b2_col=gray3; b_col=gray2; r=Top} in
Array.of_list (create_grid 10 99 2 b)

(**************MODIFIED CODE FROM OCAML GRAPHIC TUTORIAL ABOVE*****************)

(*[set_board_backgnd ()] takes in unit and setsup the background of the board*)
let set_board_backgnd () =
  let img_aa0 = to_colorAA "stratego_back_4_camel.pbm" in
  let img0 = Graphics.make_image img_aa0 in
  Graphics.draw_image img0 0 0;
  ()


(*[map_to_ui (x, y)] maps the game board state cells to the UI gameboard
 * coordinates individually*)
let map_to_ui (x, y) =
match x with
| 0 -> 9 - y
| 1 -> 19 - y
| 2 -> 29 - y
| 3 -> 39 - y
| 4 -> 49 - y
| 5 -> 59 - y
| 6 -> 69 - y
| 7 -> 79 - y
| 8 -> 89 - y
| 9 -> 99 - y
| _ -> failwith "Out of bounds."

(*[map_click_pos_x x] takes in the click x position pixel coordinate and maps
  it to the game board state cell number*)
let map_click_pos_x x =
  if (x <= 57) then 0
  else if (x < 114) then 1
  else if (x < 171) then 2
  else if (x < 228) then 3
  else if (x < 285) then 4
  else if (x < 342) then 5
  else if (x < 399) then 6
  else if (x < 456) then 7
  else if (x < 513) then 8
  else if (x < 573) then 9
  else if (x > 645 && x < 713) then 10
  else if (x > 723 && x < 762) then 11
  else 9


(*[map_click_pos_y y] takes in the click y position pixel coordinate and maps
  it to the game board state cell number*)
let map_click_pos_y y =
  if (y <= 57) then 9
  else if (y < 114) then 8
  else if (y < 171) then 7
  else if (y < 228) then 6
  else if (y < 285) then 5
  else if (y < 342) then 4
  else if (y < 399) then 3
  else if (y < 456) then 2
  else if (y < 513) then 1
  else 0

(*[click_1 click_record] takes in the click record, waits for a click, and
  updates the click record with the appropriate cell position*)
let click_1 click_record = let s = Graphics.wait_next_event
                     [Graphics.Button_down]
                     in if s.Graphics.button
                        then  (click_record.click1_x <- (map_click_pos_x
                               s.Graphics.mouse_x);
                               click_record.click1_y <- (map_click_pos_y
                               s.Graphics.mouse_y);
                               ())

(*[click_2 click_record] takes in the click record, waits for the second click,
  and updates the click record with the appropriate cell position*)
let click_2 click_record = let s = Graphics.wait_next_event
                     [Graphics.Button_down]
                     in if s.Graphics.button
                        then  (click_record.click2_x <- (map_click_pos_x
                               s.Graphics.mouse_x);
                               click_record.click2_y <- (map_click_pos_y
                               s.Graphics.mouse_y);
                               ());;

(*[check_direction click_record] takes in the current click_record and checks
  the direction of click 2 with respect to click 1*)
let check_direction click_record =
  if ((click_record.click1_x = click_record.click2_x)
     && (click_record.click1_y = click_record.click2_y)) then "invalid"
  else if ((click_record.click1_x = click_record.click2_x)
     && (click_record.click1_y < click_record.click2_y)) then "d"
  else if ((click_record.click1_x = click_record.click2_x)
     && (click_record.click1_y > click_record.click2_y)) then "u"
  else if ((click_record.click1_y = click_record.click2_y)
     && (click_record.click1_x < click_record.click2_x)) then "r"
  else if ((click_record.click1_y = click_record.click2_y)
     && (click_record.click1_x > click_record.click2_x)) then "l"
  else "invalid"

(*[string of piece] takes in the variant piece value and maps it to a string
  to be outputted on the UI*)
let string_of_piece = function
  | Spy -> "(S)"
  | Nine -> "(9)"
  | Eight -> "(8)"
  | Seven -> "(7)"
  | Six -> "(6)"
  | Five -> "(5)"
  | Four -> "(4)"
  | Three -> "(3)"
  | Two -> "(2)"
  | One -> "(1)"
  | Flag -> "(F)"
  | Bomb -> "(B)"
  | Lake -> "(L)"

(*[get_color st x y] takes in the state and given x and y position and returns
  the color of the piece corresponding to the x y position*)
let get_color st x y =
    match st.board.(x).(y) with
    | None -> Graphics.blue
    | Some f -> match f.color with
                | 0 -> Graphics.cyan
                | 1 -> Graphics.blue
                | 2 -> Graphics.red
                | _ -> failwith "Unreachable code"

(*[board_to_ui st x y ai] takes in the current state and maps the game state to
  the UI board*)
let board_to_ui st x y ai = if (ai = 1) then (draw_string_in_box Center
                                 "(?)" vb.(map_to_ui (x, y))
                                 (get_color st x y); ())
                          else (match st.board.(x).(y) with
                      |None -> (draw_string_in_box Center
                                 " " vb.(map_to_ui (x, y)) (Graphics.blue); ())
                      |Some f -> (draw_string_in_box Center
                                 (string_of_piece f.piece)
                                 vb.(map_to_ui (x, y)) (get_color st x y); ()))

(*[print_board_ui st] takes in the game state and actually prints the board to
  the UI*)
let print_board_ui st =
  let c = ref 0 in let r = ref 0 in let to_print = Array.copy (st.board) in ();
    let () = Array.iter (fun a -> r:= 0;
     Array.iter (fun b -> match b with
      | None -> (board_to_ui st !c !r 0; r:= (!r+1); ())
      | Some f -> if f.piece = Lake then (board_to_ui st !c !r 0; r:= (!r+1);
        ()) else if f.owner <> st.player_to_move then (board_to_ui st !c !r 1;
        r:= (!r+1); ())
      else
       begin match f.piece with
        | Bomb -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Spy -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Nine -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Eight -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Seven -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Six -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Five -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Four -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Three -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Two -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | One -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Flag -> board_to_ui st !c !r 0; r:= (!r+1); ()
        | Lake -> board_to_ui st !c !r 0; r:= (!r+1); ()
      end ) a; c:= (!c+1);() ) to_print in ()

  (* [split_string strin] splits string strin into its component words separated
   * by spaces (" ") *)
  let split_string strin = let str1 = String.trim strin in
                           (Str.split (Str.regexp " ") str1)

  (* [string_printer_helper str1] takes in a list of strings str1 and returns
   * a list of strings, each string containing 3 of the words in str1 *)
  let rec string_printer_helper str1 = match str1 with
  | [] -> []
  | h::[] -> [h]
  | h1::h2::[] -> [h1^" "^h2]
  | h1::h2::h3::t -> (string_printer_helper t)@[h1^" "^h2^" "^h3]

  (* [splitter str2] splits str2 into a list of strings, each containing 3 words
   * in string str2 *)
  let splitter str2 = string_printer_helper (split_string str2)

  (*[player_box] creates a box configuration for the current player*)
  let player_box = {x=645; y=520; w=65;h=25; bw=2;
    b1_col=gray1; b2_col=gray3; b_col=gray2; r=Top}

  (*[player_box_blk] creates a box configuration for updating the output
    printed message on the UI box*)
  let player_box_blk = {x=615; y=287; w=115;h=135; bw=2;
    b1_col=Graphics.black; b2_col=Graphics.black; b_col=Graphics.black; r=Top}

  (*[player_box_blk2] also creates a box configuration for updating the output
    printed message on the UI box*)
  let player_box_blk2 = {x=595; y=254; w=160;h=135; bw=2;
    b1_col=Graphics.black; b2_col=Graphics.black; b_col=Graphics.black; r=Top}

  (*[print_state_mssg st cr] takes in a state and click record and updates the
    current player output and general message output on the UI*)
  let print_state_mssg st cr=
    draw_box player_box_blk;
    draw_box player_box_blk2;
    cr.msg_pointer <- 310;
    Graphics.moveto 623 cr.msg_pointer;
    Graphics.set_color Graphics.white;


    let lst2 = splitter st.to_print in
    List.fold_left (fun acc elem ->
      Graphics.draw_string elem;
      cr.msg_pointer <- (cr.msg_pointer + 12);
      Graphics.moveto 623 cr.msg_pointer;  ()) () lst2;

    let lst1 = splitter cr.player_1 in
    List.fold_left (fun acc elem ->
      Graphics.draw_string elem;
      cr.msg_pointer <- (cr.msg_pointer + 12);
      Graphics.moveto 623 cr.msg_pointer;  ()) () lst1;


    Graphics.moveto 656 535;
    draw_box player_box;
    if (st.player_to_move = 1) then
    draw_string_in_box Center "PLAYER 1" player_box Graphics.white
    else draw_string_in_box Center "PLAYER 2" player_box Graphics.white;
    ()



(************************************************************************)
(************************************************************************)
(***********************UI/GRAPHICS CODE ABOVE***************************)
(************************************************************************)
(************************************************************************)

let opener = "Welcome to Stratego. Press 1 to begin a 1 player Game,
press 2 to begin a 2 player game."

let explanation_one_player = "The goal of Stratego is to capture the opponent’s flag.\nEach player has 40 pieces, composed of his own flag, bombs, and his army,\nwhich contains his Marshal (1), Generals (2), Colonels (3),\nMajors (4), Captains (5), Lieutenants (6), Sergeants (7),\nMiners (8), Scouts (9) and a Spy (S).
Each piece has a strength, based on their corresponding numbers.\nA Marshal can capture a General or anything weaker, a General can capture\na Colonel or anything weaker and so on. The only exceptions\nare Spies who when attacked, always lose, but\nwhen attacking a Marshal, will win, and Miners,\nwhich are the only pieces that can destroy bombs.
\nIn each turn, each player can move one of his pieces (excluding his flag or\nhis bombs, which cannot move) one space, either Up, Down Left or Right.\nThere are impassable lakes in the middle of the board\nwhich no piece can move to. The pieces are set up so that neither\nplayer can see what the other player’s pieces are,\nonly that they are there, and when pieces attack each other,\ntheir rank is revealed. The game ends and a\nplayer wins when the player has captured the other player’s flag, or when the\nother player has no movable pieces left.
\nLet me introduce you to the board. The vertical axis is the y-axis, starting from
0 at the top and 9 at the bottom. The horizontal axis is the x-axis, 
also labeled 0 through 9, from left to right.\nAll points can be referred to by their (y,x) coordinates.
To advance a piece, click on its square and then click the square you want it to move to.
At anytime, click QUIT to stop the game.\n\nNow, you will place your pieces on your half of the board.
Player 1 can place pieces in rows 6 through 9. For each piece, input the y coordinate and
x coordinate of where you would like it to be placed.
For example, \"Bomb > 6 7\" will place it at row 6 column 7.
Input R and a random setup will be done for you."

let next_explanation = "Player 1 begins the game. enter a command to begin. Again, to advance a piece click on its square and the proceeding square."

let try_again = "Sorry, I didn't catch that. Please try again."

(* ALL OF THE PIECES FOR PLAYER 1 *)

let piecelist1 = [Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Bomb; seen=false; owner=1; color=1};
                 Some {piece=Flag; seen=false; owner=1; color=1};
                 Some {piece=Spy; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Nine; seen=false; owner=1; color=1};
                 Some {piece=Eight; seen=false; owner=1; color=1};
                 Some {piece=Eight; seen=false; owner=1; color=1};
                 Some {piece=Eight; seen=false; owner=1; color=1};
                 Some {piece=Eight; seen=false; owner=1; color=1};
                 Some {piece=Eight; seen=false; owner=1; color=1};
                 Some {piece=Seven; seen=false; owner=1; color=1};
                 Some {piece=Seven; seen=false; owner=1; color=1};
                 Some {piece=Seven; seen=false; owner=1; color=1};
                 Some {piece=Seven; seen=false; owner=1; color=1};
                 Some {piece=Six; seen=false; owner=1; color=1};
                 Some {piece=Six; seen=false; owner=1; color=1};
                 Some {piece=Six; seen=false; owner=1; color=1};
                 Some {piece=Six; seen=false; owner=1; color=1};
                 Some {piece=Five; seen=false; owner=1; color=1};
                 Some {piece=Five; seen=false; owner=1; color=1};
                 Some {piece=Five; seen=false; owner=1; color=1};
                 Some {piece=Five; seen=false; owner=1; color=1};
                 Some {piece=Four; seen=false; owner=1; color=1};
                 Some {piece=Four; seen=false; owner=1; color=1};
                 Some {piece=Four; seen=false; owner=1; color=1};
                 Some {piece=Three; seen=false; owner=1; color=1};
                 Some {piece=Three; seen=false; owner=1; color=1};
                 Some {piece=Two; seen=false; owner=1; color=1};
                 Some {piece=One; seen=false; owner=1; color=1};]

(* ALL OF THE PIECES FOR PLAYER TWO *)
let piecelist2 = [Some {piece=Bomb; seen=false; owner=2; color=2};
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

(* [check_winner st] is a method that updates st.winner to 1 if player 2 can 
 * no longer move to 2 if player 1 can no longer move, and 3 if the game 
 * has ended in a draw *)
let check_winner st =
  let p1_al = Game.p_alive 1 st in let p2_al = Game.p_alive 2 st in if (p1_al)&&
    (p2_al) then () else if (not (p1_al))&&(not (p2_al)) then st.winner <- 3 
      else if not (p1_al) then st.winner <- 2 else st.winner <- 1

(* [print_board st] prints st.board as a string in utop, with each piece 
 * represented as its number.'?' is printed if it belongs to the player 
 * not currently moving. '-' represents an empty space. *)
let print_board st =
  let c = ref 0 in let to_print = Array.copy (st.board) in print_newline ();
    let () = Array.iter (fun a -> print_string ("["^(string_of_int !c)^"] ");
    c:= (!c+1); Array.iter (fun b -> match b with
      | None -> print_string "-  "
      | Some f -> if f.piece = Lake then print_string "L  " else 
      if f.owner <> st.player_to_move then print_string "?  " else
      begin match f.piece with
        | Bomb -> print_string "B  "
        | Spy -> print_string "S  "
        | Nine -> print_string "9  "
        | Eight -> print_string "8  "
        | Seven -> print_string "7  "
        | Six -> print_string "6  "
        | Five -> print_string "5  "
        | Four -> print_string "4  "
        | Three -> print_string "3  "
        | Two -> print_string "2  "
        | One -> print_string "1  "
        | Flag -> print_string "F  "
        | Lake -> print_string "L  "
      end ) a; print_newline () ) to_print in let () = 
      print_string "   [0][1][2][3][4][5][6][7][8][9]\n\n" in ()

(* [shuffle_piece d] is a shuffled list of the full_pieces in list d*)
let shuffle_piece d = Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

(* [setup_random1 st] is a method that randomly sets up the pieces for player 1,
 * only in rows 6 through 9. *)
let setup_random1 st =
  let x = ref 6 in let y = ref 0 in
  let _ = List.map (fun current_piece -> if (!y < 10) then
  begin st.board.(!x).(!y) <- current_piece; y := !y + 1 end
  else begin (x:=!x+1); (y := 0);  st.board.(!x).(!y) <- current_piece;
  y := !y+1 end) (shuffle_piece piecelist1) in ()

(* [setup_random2 st] is a method that randomly sets up the pieces for player 2,
 * only in rows 0 through 3. *)
let setup_random2 st =
  let x = ref 0 in let y = ref 0 in
  let _ = List.map (fun current_piece -> if (!y < 10) then
  begin st.board.(!x).(!y) <- current_piece; y := !y + 1 end
  else begin (x:=!x+1); (y := 0);  st.board.(!x).(!y) <- current_piece;
  y := !y+1 end) (shuffle_piece piecelist2) in ()

(* [place_pieces1 st piece_list] is a method that takes input from player 1,
 * particularly, a (y,x) position, and places that piece on the board if the 
 * location is valid. That position must be empty and it must be within 
 * rows 6 through 9. By entering 'R' the board will be set up randomly.
 * precondition: piece_list is a list of full_pieces. *)
let rec place_pieces1 st piece_list =
  if st.quit then begin print_string "\n\nThanks for playing!\n"; exit 0 end else
  match piece_list with
  | [] -> ()
  | h :: t -> begin
    match h with
    | None -> ()
    | Some f -> let () = print_string ((string_of_piece f.piece)^" > ") in 
      let c = read_line () in if String.trim (String.lowercase_ascii c) = "quit"
        then begin st.quit <- true;
        place_pieces1 st (h::t) end else 
          if String.trim (String.lowercase_ascii c) = "r" then
          begin setup_random1 st; print_board st; () end else
        try (let y = int_of_string (String.sub c 0 1) in 
          let x = int_of_string (String.sub c 2 1) in
        if (y<6)||(y>9) then begin print_endline "Invalid, try again."; 
          place_pieces1 st (h::t) end else
          match st.board.(y).(x) with
          | None -> st.board.(y).(x) <- h; print_board st; place_pieces1 st t
          | Some f -> print_endline "Piece already there."; 
            place_pieces1 st (h::t) )
    with _ -> print_endline "Invalid, try again.\n"; place_pieces1 st (h::t)
  end

(* [place_pieces2 st piece_list] is a method that takes input from player 2,
 * particularly, a (y,x) position, and places that piece on the board if the 
 * location is valid. That position must be empty and it must be within 
 * rows 0 through 3. By entering 'R' the board will be set up randomly.
 * precondition: piece_list is a list of full_pieces. *)
let rec place_pieces2 st piece_list =
  if st.quit then begin print_string "\n\nThanks for playing!\n"; exit 0 end else
  match piece_list with
  | [] -> ()
  | h :: t -> begin
    match h with
    | None -> ()
    | Some f -> let () = print_string ((string_of_piece f.piece)^" > ") in 
      let c = read_line () in if String.trim (String.lowercase_ascii c) = "quit"
        then begin st.quit <- true;
        place_pieces2 st (h::t) end else 
          if String.trim (String.lowercase_ascii c) = "r" then
          begin setup_random2 st; print_board st; () end else
        try (let y = int_of_string (String.sub c 0 1) in 
          let x = int_of_string (String.sub c 2 1) in
        if (y<0)||(y>3) then begin print_endline "Invalid, try again."; 
          place_pieces1 st (h::t) end else
          match st.board.(y).(x) with
          | None -> st.board.(y).(x) <- h; print_board st; place_pieces2 st t
          | Some f -> print_endline "Piece already there."; 
            place_pieces2 st (h::t) )
    with _ -> print_endline "Invalid, try again.\n"; place_pieces2 st (h::t)
  end


(* REPL for one player mode
 * First the REPL checks if a winner has been determined by calling 
 * [check_winner}. Then it checks if st.quit is true, meaning a user has 
 * inputted "QUIT". Then it takes user input, and it tries to execute 
 * Game.advance with this (y,x) position and direction. The user can input "r" 
 * to do a random move, specifically, a move that the AI would do.
 * Then, the board is updated and printed to the user before calling 
 * Ai.advance_ai, in which the AI makes a move. 
 * Then, the board is updated again and another call is made to repl4.
 *
 * If at any point the user inputs a move that cannot be
 * understood or is invalid, they are made aware. *)
  let rec repl4 st =
   check_winner st; if st.winner = 1 then begin print_board_ui st; 
    print_board st; print_string st.to_print;
    print_string "\n\nYou win!\n\n"; exit 0 end else
  if st.winner = 2 then begin print_board_ui st; print_board st; 
    print_string st.to_print;
    print_string "\n\nYou lose.\n\n"; exit 0 end else
  if st.winner = 3 then begin print_board_ui st; print_board st; 
    print_string st.to_print;
    print_string "\n\nIt's a draw!\n\n"; exit 0 end else
  if st.quit then begin print_string "\n\nThanks for playing!\n"; exit 0 end else
  (if st.player_to_move = 2 then begin st.player_to_move <- 1; 
    print_board_ui st; print_state_mssg st click_rec; 
      click_rec.player_1 <- st.to_print;
    st.player_to_move <- 2; Ai.advance_ai st; repl4 st end else
    Array.iter draw_box vb; print_board_ui st; print_state_mssg st click_rec;
    click_1 click_rec;
    if (click_rec.click1_x = 11 && click_rec.click1_y = 0) then 
      begin st.quit <- true; repl4 st end
    else if (click_rec.click1_x = 10 && click_rec.click1_y = 1) then 
      begin let ((y1,x1),dir1)=(choose_move st) in 
        Game.advance st (y1,x1) dir1; repl4 st end
    else
    click_2 click_rec;
    try (let y = click_rec.click1_y in
    let x = click_rec.click1_x in
    let dir = check_direction click_rec in
      match dir with
        | "u" -> Game.advance st (y,x) Up; repl4 st
        | "d" -> Game.advance st (y,x) Down; repl4 st
        | "l" -> Game.advance st (y,x) Left; repl4 st
        | "r" -> Game.advance st (y,x) Right; repl4 st
        | _ -> st.to_print <- try_again; repl4 st)
    with
    | _ -> st.to_print <- try_again; repl4 st)


(* REPL for two player mode - erase_box (map_to_ui (y,x)); 
 * erase_box (map_to_ui ((y-1),x));
 * First the REPL checks if a winner has been determined by calling 
 * [check_winner].
 * Then it checks if st.quit is true, meaning a user has inputted "QUIT".
 * Then it takes user input, and it tries to execute Game.advance with this 
 * (y,x) position and direction. The user can input "r" to do a random move, 
 * specifically, a move that the AI would do.
 * Then, the board is updated and printed to the user before calling repl3
 * If the first player made a valid move, Game.ml will have changed 
 * the player_to_move and player 2 can make his/her move, other wise the 
 * player_to_move will not change
 * and the appropriate st.to_print will be outputted.
 *
 * If at any point the user inputs a move that cannot be
 * understood or is invalid, they are made aware. *)
let rec repl3 st =
  check_winner st; if st.winner = 1 then begin print_board_ui st; 
    print_board st; print_string st.to_print;
    print_string "\n\nPlayer 1 wins!\n\n"; exit 0 end else
  if st.winner = 2 then begin print_board_ui st; print_board st; 
    print_string st.to_print;
    print_string "\n\nPlayer 2 wins!\n\n"; exit 0 end else
  if st.winner = 3 then begin print_board_ui st; print_board st; 
    print_string st.to_print;
    print_string "\n\nIt's a draw!\n\n"; exit 0 end else
  if st.quit then begin print_string "\n\nThanks for playing!\n"; exit 0 end 
  else begin
    (Array.iter draw_box vb; print_board_ui st; print_state_mssg st click_rec;
    click_1 click_rec;
    if (click_rec.click1_x = 11 && click_rec.click1_y = 0) then 
    begin st.quit <- true; repl3 st end
    else if (click_rec.click1_x = 10 && click_rec.click1_y = 1) then 
      begin let ((y1,x1),dir1)=(choose_move st) in 
        Game.advance st (y1,x1) dir1; repl3 st end
    else
    click_2 click_rec;
    try (let y = click_rec.click1_y in
    let x = click_rec.click1_x in
    let dir = check_direction click_rec in
      match dir with
        | "u" -> Game.advance st (y,x) Up; repl3 st
        | "d" -> Game.advance st (y,x) Down; repl3 st
        | "l" -> Game.advance st (y,x) Left; repl3 st
        | "r" -> Game.advance st (y,x) Right; repl3 st
        | _ -> st.to_print <- try_again; repl3 st)
    with
  | _ -> st.to_print <- try_again; repl3 st) end

(* Begins the appropriate Graphics UI based on the game mode. *)
let choose_game_mode_helper x st =
  Graphics.open_graph " 780x640+275-200";
  Array.iter draw_box vb;
  set_board_backgnd ();
  match x with
  |3 -> repl3 st
  |4 -> repl4 st
  | _ -> failwith "Invalid Option"

(* After the game mode has been selected, more information is outputted to the 
 * player(s) then the correct place_pieces function is called to either have 
 * both players place their pieces or one player and the AI. *)
let rec repl2 st =
  print_string ("\n"^st.to_print^"\n\n"); print_board st; 
    place_pieces1 st piecelist1; if st.quit then begin print_string 
      "\n\nThanks for playing!\n"; () end else if st.mode = Two_Player then
    begin st.player_to_move <- 2; print_board st; print_endline "Now player 2 places pieces. You can only use rows 0 through 3.\nEnter the vertical coordinate, then the horizontal coordinate."; place_pieces2 st piecelist2; st.player_to_move <- 1;
    print_endline "It's time to start!"; st.to_print <- next_explanation; 
      choose_game_mode_helper 3 st end
    else begin Ai.setup_random st; print_board st; 
      print_endline "It's time to start!"; st.to_print <- next_explanation;
      choose_game_mode_helper 4 st end

(* [repl1] welcomes the player(s) to the game, allows the user to decide on 
 * either 1 or 2 player mode, and displays important information about how to 
 * move through the game. *)
let rec repl1 st =
  if st.quit then exit 0 else
  let () = print_string ("\n"^st.to_print^"\n\n") in
  let c1 = read_line () in if String.trim (String.lowercase_ascii c1) = "quit" 
    then begin st.quit <- true; print_string "\nGoodbye.\n"; () end else
  if c1 = "1" then begin st.mode <- One_Player; 
    st.to_print <- explanation_one_player;
  repl2 st end else if c1 = "2" then begin st.mode <- Two_Player; 
    st.to_print <- explanation_one_player; repl2 st end
  else st.to_print <- try_again; repl1 st

