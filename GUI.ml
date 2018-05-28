open Terminal_UI
open Board
open GMain
open GdkKeysyms

let locale = GtkMain.Main.init ()

(* messages for options menu to display *)
let game_choice_text =
  "How do you want to play?\n" ^
  "Your game will begin in the command line when you choose an option."

let game_started_text =
  "If you want to start another game, press one of these buttons."

(* Instructions and Controls are two different pages *)
let instruction_text = get_string_from_file
    (directory_name ^ "summary.txt")

let control_text = get_string_from_file
    (directory_name ^ "instructions.txt")

let credits_text =
  "OCaml Stratego\n" ^
  "Created for CS 3110\n\n" ^
  "John Kolesar\n" ^
  "Kevin Li\n" ^
  "Christian Rodriguez\n" ^
  "Anthony Verghese"

(* true when using size 10, false when using size 8 *)
let current_size : bool ref = ref false

(**
 * Several buttons in the GUI behave in the same manner:  clicking on them
 * causes one component of the GUI to be closed and another component to be
 * opened.  [switch_view c o] creates functions for buttons of this type
 * to use.
 *
 * Do not apply this function with all three of its arguments within this
 * file.
 * [c] is the view to be closed.
 * [o] is the view to be opened.
 *)
let switch_view c o = fun () ->
  c#misc#hide ();
  o#misc#show ()

(**
 * Configures a button to switch the view of the GUI between two different
 * components.  Returns a unit.
 *)
let make_switch_button b c o =
  ignore (b#connect#clicked ~callback:(switch_view c o))

let main () =

  clear_screen ();

  (* Width and height here specify the minimum window size.  It can be
   * expanded either vertically or horizontally by dragging. *)
  let window = GWindow.window ~width:550 ~height:550
      ~title:"Stratego" () in
  let vbox = GPack.vbox
      ~packing:window#add () in
  (* when the window is closed, the program should terminate *)
  ignore (window#connect#destroy ~callback:Main.quit);

  (* This GUI uses different vbox elements to display different screens. *)
  let vbox_menu = GPack.vbox
      ~packing:vbox#add
      ~show:true () in

  (* Include some sort of "title screen logo" on the main menu *)
  let logo = GMisc.image ~file:"stratego_0.png"
      ~packing:vbox_menu#add () in

  (* the menu consists of several buttons *)

  let button_play = GButton.button ~label:"Start Game"
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) () in
  let button_help = GButton.button ~label:"Instructions"
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) () in
  let button_control = GButton.button ~label:"Controls"
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) () in
  let button_cred = GButton.button ~label:"Credits"
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) () in

  (* final button:  exit the program *)
  let button_quit = GButton.button ~label:"Quit"
      ~packing:(vbox_menu#pack ~expand:true ~fill:true) () in
  ignore (button_quit#connect#clicked ~callback:Main.quit);

  (* Game Options Page *)
  let vbox_opt = GPack.vbox
      ~packing:vbox#add
      ~show:false () in

  let opt_text = GMisc.label ~text:game_choice_text
      ~packing:(vbox_opt#pack ~expand:true ~fill:true) () in

  (* When the user moves to the game options page, the text there needs
   * to be reset to the default. *)
  ignore (button_play#connect#clicked ~callback:(fun () ->
      opt_text#set_label game_choice_text;
      switch_view vbox_menu vbox_opt ()));

  (* One button for each board size and number of players *)
  let start10_2 = GButton.button ~label:"Traditional Game (2P)"
      ~packing:(vbox_opt#pack ~expand:true ~fill:true) () in

  let start8_2 = GButton.button ~label:"Quick Match (2P)"
      ~packing:(vbox_opt#pack ~expand:true ~fill:true) () in

  let start10_1 = GButton.button ~label:"Traditional Game (1P)"
      ~packing:(vbox_opt#pack ~expand:true ~fill:true) () in

  let start8_1 = GButton.button ~label:"Quick Match (1P)"
      ~packing:(vbox_opt#pack ~expand:true ~fill:true) () in

  (* These buttons start two-player games. *)
  ignore (start10_2#connect#clicked ~callback:(fun () ->
      opt_text#set_label game_started_text;
      CL10.play_2 ()));

  ignore (start8_2#connect#clicked ~callback:(fun () ->
      opt_text#set_label game_started_text;
      CL8.play_2 ()));

  (* After choosing a single-player game, the user has the option to play
   * as Red or Blue. *)
  let vbox_choose_color = GPack.vbox
      ~packing:vbox#add
      ~show:false () in

  (* the buttons in this sub-menu do the actual work of starting the game *)
  ignore (start10_1#connect#clicked ~callback:(fun () ->
      current_size := true;
      switch_view vbox_opt vbox_choose_color ()));
  ignore (start8_1#connect#clicked ~callback:(fun () ->
      current_size := false;
      switch_view vbox_opt vbox_choose_color ()));

  let choose_color_info = GMisc.label ~text:"Choose a Color for Yourself"
      ~packing:(vbox_choose_color#pack ~expand:true ~fill:true) () in

  let choose_red = GButton.button ~label:"Red (Moves First)"
      ~packing:(vbox_choose_color#pack ~expand:true ~fill:true) () in
  let choose_blue = GButton.button ~label:"Blue (Moves Second)"
      ~packing:(vbox_choose_color#pack ~expand:true ~fill:true) () in
  (* button to cancel at this point *)
  let done_with_choose_color = GButton.button ~label:"Back"
      ~packing:(vbox_choose_color#pack ~expand:false ~fill:true) () in

  (* this is where the single-player buttons start the games *)
  ignore (choose_red#connect#clicked ~callback:
            ((if !current_size then CL10.play_ai else CL8.play_ai) Red));

  ignore (choose_blue#connect#clicked ~callback:
            ((if !current_size then CL10.play_ai else CL8.play_ai) Blue));

  make_switch_button done_with_choose_color vbox_choose_color vbox_opt;

  (* cancel the creation of a new game *)
  let done_with_opt = GButton.button ~label:"Back"
      ~packing:(vbox_opt#pack ~expand:false ~fill:true) () in
  make_switch_button done_with_opt vbox_opt vbox_menu;

  (* Instructions Page *)
  let vbox_help = GPack.vbox
      ~packing:vbox#add
      ~show:false () in
  make_switch_button button_help vbox_menu vbox_help;

  let help_info = GMisc.label ~text:instruction_text
      ~packing:(vbox_help#pack ~expand:true ~fill:true) () in

  (* help page needs a button to return to menu *)
  let done_with_help = GButton.button ~label:"Back"
      ~packing:(vbox_help#pack ~expand:false ~fill:true) () in
  make_switch_button done_with_help vbox_help vbox_menu;

  (* controls page is separate from general instructions page *)
  let vbox_control = GPack.vbox
      ~packing:vbox#add
      ~show:false () in
  make_switch_button button_control vbox_menu vbox_control;

  let control_info = GMisc.label ~text:control_text
      ~packing:(vbox_control#pack ~expand:true ~fill:true) () in

  let done_with_control = GButton.button ~label:"Back"
      ~packing:(vbox_control#pack ~expand:false ~fill:true) () in
  make_switch_button done_with_control vbox_control vbox_menu;

  (* credits page is simple *)
  let vbox_cred = GPack.vbox
      ~packing:vbox#add
      ~show:false () in
  make_switch_button button_cred vbox_menu vbox_cred;

  (* cool CS 3110 logo on credits page *)
  let cred_image = GMisc.image ~file:"cs3110large_0.png"
      ~packing:vbox_cred#add () in

  let cred_info = GMisc.label ~text:credits_text
      ~packing:(vbox_cred#pack ~expand:true ~fill:true) () in

  let done_with_cred = GButton.button ~label:"Back"
      ~packing:(vbox_cred#pack ~expand:false ~fill:true) () in
  make_switch_button done_with_cred vbox_cred vbox_menu;

  (* this does the actual work of displaying the window *)
  window#show ();
  Main.main ()

let () = main ()
