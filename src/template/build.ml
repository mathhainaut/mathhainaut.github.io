(** build.ml inspired from OCaml's website build.ml:
https://github.com/ocaml/ocaml.org/blob/master/script/build.ml
*)

open Printf
open Nethtml
module Path = Weberizer.Path

let tpl = Canvas.empty
(* To avoid symlinks for the images but still share them accross the
various translations of the website, one need to change the paths
of the images of translated sites. *)

let rec drop_last = function
  | [] | [_] -> []
  | a :: tl -> a :: drop_last tl

let dir_from_base p =
  let path = Path.from_base_split p in
  if Path.filename p = "" then path else drop_last path

(* [img_dir]: path to images from the location [p] points to. *)
let modify_img_path ~img_dir p ((src, url) as arg) =
  let url = Neturl.split_path url in
  let path = Neturl.norm_path(dir_from_base p @ url) in
  match path with
  | "img" :: sub_path ->
     let url' = img_dir ^ Neturl.join_path sub_path in
     (src, url')
  | _ -> arg

let rec img_path_translations ~img_dir p html =
  List.map (modify_img_path_element ~img_dir p) html

and modify_img_path_element ~img_dir p = function
  | Nethtml.Element("img", args, content) ->
     let src, args = List.partition (fun (a,_) -> a = "src") args in
     let src = List.map (modify_img_path ~img_dir p) src in
     Nethtml.Element("img", src @ args, content)
  | Nethtml.Element(e, args, content) ->
     Nethtml.Element(e, args, img_path_translations ~img_dir p content)
  | Nethtml.Data _ as e -> e

let make_link link ?(args=[]) text =
  Nethtml.Element("a", ("href", link)::args , [Nethtml.Data text])


let stop_on_error = ref false
let spec = [
  ("--stop-on-error", Arg.Set stop_on_error,
   " stop the build if an error is encountered");
]

let () =
  eprintf "Starting generation\n%!";
  Arg.parse (Arg.align spec) (fun _ -> raise(Arg.Bad "no anonymous arguments"))
	    "build <options>";
  let b =  Weberizer.Binding.make() in
  let module B =  Weberizer.Binding in
  begin
    if !stop_on_error then
      B.on_error b (fun v a e -> raise e);
    B.string b "map" Settings.map;
    B.string b "map_options" Settings.map_options;
    B.fun_html b "timetable" Html_timetable.timetable;
    B.fun_html b "form_timetable" Html_formations.timetable;
    B.fun_html b "graph" Html_timetable.graph;
    B.html b "form_graph" Html_formations.graph;
    B.string b "date_of_update" (Date.format_t (Date.today()));
  end;
  let langs = [Settings.main_lang] in
  let out_dir lang = if lang = Settings.main_lang then "./" else lang in

  let is_base_file file =
    print_string file;
    let b = Filename.dirname file = Settings.sources (*List.mem file (List.map fst Settings.page_order*) in
    print_string (if b then " base" else " not");
    print_newline ();
    b
  in

  let webpage_title current =
    List.assoc (Filename.dirname current)
	       Settings.(
      [sourcedir main,
       Nethtml.([Element("h1",["id","project_title"],
			 [Element("a",["href","index.html"],
				  [Data("Math Hainaut")])
			 ]
			)]
	       );
       sourcedir uvhc,
       Nethtml.([Element("h1",["id","project_title";
			       "style","font-color:white"],
			 [Nethtml.Data("UVHC - ESPE")]
			)]
	       );
       sourcedir labbe, []
      ])
  in
  let files_array = Sys.readdir Settings.(sourcedir main) in
  let files_list = List.filter (fun file ->
				Filename.check_suffix file "html"
			       ) (Array.to_list files_array)
  in
  let nref = ref 10 in
  let make_tuple s =
    let n =
      try
	List.assoc s Settings.page_order
      with Not_found ->
	nref := !nref + 1;
	!nref
    in
    n, s, (
      let full = Settings.(sourcedir main)^"/"^s in
      let webfile = Weberizer.read full ~bindings:b in
      Weberizer.title_of webfile)
  in
  let pages_titles = List.rev_map make_tuple files_list
  in
  let pages_titles = List.sort (fun (a,_,_) (b,_,_) -> b - a) pages_titles in
  let create_menu current =
    let treat_pair list (_, file, title) =
      let list = if list = [] then []
		 else ( Nethtml.Data " | ") :: list
      in
      let args = [("id",
		   if title = current then "current_page" else "other_page")]
      in
      (make_link file ~args title) :: list
    in
    let h2_args = [("id","other_page")] in
    let h2_content = List.fold_left treat_pair [] pages_titles
    in
    [Nethtml.Element("h2",h2_args, h2_content)]
  in

  let filter _ = true in
  let process_html lang p =
    eprintf "Processing %s\n%!" (Path.full p);
    let url_base =
      if Path.in_base p then ""
      else Path.to_base p
    in
    Weberizer.Binding.string b "url_base" url_base;
    let full_path = Path.full p in
    let is_main = Filename.dirname full_path = Settings.(sourcedir main) in
    
    let page = Weberizer.read full_path ~bindings:b in
    let title = Weberizer.title_of page in
    let tpl = Canvas.title tpl title in

    let prefix = if lang = Settings.main_lang then "" else "../" in

    let stylesheet_loc = url_base ^ prefix^ Settings.stylesheet in
    let tpl = Canvas.stylesheet tpl stylesheet_loc in

    let webpage_title = webpage_title full_path in
    let tpl = Canvas.webpage_title tpl webpage_title in
    let img_dir = url_base ^ prefix ^ Settings.img_dir in

    let menu = if is_main then create_menu title else [] in
    let tpl = Canvas.menu tpl menu in

    let body = Weberizer.body_of page in
    let body = Weberizer.protect_emails body in
    let body = img_path_translations p body ~img_dir in
    let tpl = Canvas.main tpl body in

    let subject = "Contact from webpage "^(Path.from_base p)^"/"^(Path.filename p) in
    let subject = "?subject="^subject in
    let email = Weberizer.email (Settings.email^subject) in
    let tpl = Canvas.email tpl email in
    Canvas.render tpl
  in
  Weberizer.iter_html ~filter ~langs Settings.sources ~out_dir process_html
		      ~perm:0o755
