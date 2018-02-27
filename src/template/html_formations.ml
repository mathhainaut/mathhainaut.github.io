open Nethtml
open Timetable

let text_of_date date =
  Date.(Printf.sprintf "%a %s" (fun () -> function
                                         Mon -> "Lundi"
                                       | Tue -> "Mardi"
                                       | Wed -> "Mercredi"
                                       | Thu -> "Jeudi"
                                       | Fri -> "Vendredi"
                                       | Sat -> "Samedi"
                                       | Sun -> "Dimanche")
                       (weekday date) (format_t date))


let formations = merge [formations;
			split_elements [form "Rech"
					     Date.(
					   [[Jan 24; Feb 14; Mar 21; Apr 4]
					    => (13|:30, 16|:30)])
				       ]
		       ]

let gather_data s dateref =
  let cmp =
    match s with (* Beware on reading! *)
    | ["done"] -> (fun d ->  d <= 0)
    | ["next"] -> (fun d ->  d > 14)
    | ["coming"] -> (fun d -> (d >= 0 && d <= 14))
    | ["graph_current"] -> (fun d -> (d >= 0 && d < 7))
    | ["graph_coming"] -> (fun d -> (d >= 7 && d < 14))
    | ["graph_next"] -> (fun d -> (d >= 14 && d < 21))
    | _ -> invalid_arg "Html_formation.gather_data"
  in
  let filterer agenda =
    cmp (Date.diff_days dateref agenda.date)
  in
  List.filter filterer formations

let data_elt s =
  [Nethtml.Data s]

let tag ?(args=[]) ?(content=[]) tag = Nethtml.Element(tag,args,content)

let timetable _ s =
  let all_html =
    List.fold_left
      (fun html agenda ->
       let html_date = data_elt (Date.format_t agenda.date)
       in
       let html_events =
         List.fold_left
           (fun html2 event ->
	    let line =
	      Printf.sprintf "%s-%s : %s%s, %s"
			     (Date.format_s event.from)
			     (Date.format_s event.till)
			     event.data
			     (match event.precision with
				"" -> ""
			      | s -> " ("^s^")")
			     event.location
            in
            let content = data_elt line in
	    (tag "li" ~content)::html2
	   )
           [] agenda.events
       in
       match html_events with
	 [] -> html
       | _ ->
	  let content =
	    List.rev ((tag "ul" ~content:html_events) :: html_date)
	  in
	  let li = [tag "li" ~content] in
	  let args =
            [
              "style","javascript:setDisp("^(Date.format_t ~sep:"-" agenda.date)^")"
            ]
	  in
	  (tag "span" ~args ~content:li) :: html
      )
      [] (gather_data s (Date.today()))
  in
  [tag "ul" ~content:all_html]

let jan1 = Date.(make (Jan 1, 2018))
		  
let weeknum d =
  let diff = Date.diff_days jan1 d in
  diff / 7 +1

let dates w =
  let d1 = Date.add_days jan1 (7*(w-1)) in
  let wed = Date.next ~d:Date.Wed d1 in
  let digits_wed = String.sub (Date.format wed) 0 2 in
  digits_wed^" et "^(Date.format_t (Date.next wed))

let add_color ue =
  let list =
    ["Didac", "rgb(255,128,0)";
     "ContEx", "rgb(0,128,0)";
     "LV", "rgb(0,96,0)";
     "SitPro", "blue";
     "HisMaths", "rgb(96,96,0)";
     "Immersion", "rgb(0,192,0)";
     "TICE",  "rgb(64,128,0)";
     "Rech", "rgb(192, 64, 0)"]
  in
  let s =
    try List.assoc ue list
    with Not_found ->
      print_string ue;
      print_string " not found \n";
      "blue"
  in
  "style", "fill:"^s^";fill-opacity:0.5"
		       
let graph =
  let weekdays = ["Lundi";"Mardi";"Mercredi";"Jeudi";"Vendredi";"Samedi";"Dimanche"] in
  let width = 480 and height = 600 in
  let week_height = 22
  and header_height = 50
  and top_margin = 0 in
  let day_width = 180
  and margin = 120 in
  let left_padding = 30
  and header_position = 15
  and header_position2 = 45
  and week_label_position = 0
  and week_alignment = 15 in
  let weeks_extent = 25 in
  let main_lines =
    let desc =
      let rec f i l =
	if i > weeks_extent then l
	else f (i+1)
	       ((true, header_height + week_height*(i)) :: l)
	       
      in (false, margin + 2*day_width)
	 ::(false, margin + day_width)
	 ::(false, margin)
	 :: (f 0 [])
    in
    List.rev_map (fun (horiz, level) ->
		  let args1 =
		    ["x1", if horiz then margin else level;
		     "y1", if horiz then level else top_margin;
		     "x2", if horiz then width else level;
		     "y2", if horiz then level else height]
		  in
		  let args =
		    ("style", if not horiz || level = header_height then
				"stroke:rgb(0,0,0);stroke-width:2"
			      else
				"stroke:rgb(64,64,64);stroke-width:1" )
		    :: List.rev_map (fun (s,i) -> s, string_of_int i) args1
		  in
		  tag "line" ~args) desc
  in
  let second_lines =
    let rec f i l =
      if i = 10 then f (i+1) l
      else if i >= 20 then l
      else f (i+1) (string_of_int (margin + i * day_width / 10)::l)
    in
    List.rev_map (fun x ->
		  let args =
		    ["style", "stroke:rgb(192,192,192);stroke-width:1";
		     "x1",x; "x2",x;
		     "y1", string_of_int header_position2;
		     "y2", string_of_int height]
		  in tag "line" ~args) (f 1 [])
  in		  
  let col_titles =
    let days =
      List.mapi (fun i d ->
		 let args =
		   ["x", string_of_int (margin+day_width*i+left_padding);
		    "y", string_of_int header_position;
		   ]
		 in
		 tag "text" ~args ~content:(data_elt d)) ["Mercredi"; "Jeudi"]
    and hours =
      let rec f h l =
	if h >= 10 then l
	else
	  let args i =
	    ["x", string_of_int (margin+day_width*i+h*day_width/10 - 4);
	     "y", string_of_int header_position2;
	     "font-size","10" ]
	  in
	  let content = data_elt (string_of_int (h+8)) in
	  let text args = tag "text" ~args ~content in
	  let l' = text (args 0) :: text (args 1) :: l in
	  f (h+1) l'
      in f 1 []
    in List.append days hours
  in
  let line_titles =
    let elt_week i =
      let level = header_height + week_height*i + week_alignment in
      let args =
	["x", string_of_int week_label_position;
	 "y", string_of_int level;
	 "font-size","12"]
      in
      let s = Printf.sprintf "%d : %s" (i+1) (dates (i+1)) in
      tag "text" ~args ~content:(data_elt s)
    in
    Array.to_list (Array.init weeks_extent elt_week)
  in
  let blocks =
    let time_level thu time =
      let h = time.Date.h - 8 in
      if h < 0 || h > 10 then
	let s = string_of_int h in
	invalid_arg ("Html_formations.graph: time_level hour="^s)
      else
	let left_limit =
	  if thu then margin+day_width
	  else margin
	in
	left_limit + h* (day_width/10) + time.Date.m * day_width / 600 
    in
    List.fold_left
      (fun svg agenda ->
       let w = weeknum agenda.date in
       if w > 0 then
         let vert =
	   header_height + week_height * (w-1)
	 in
	 let thu = Date.weekday agenda.date = Date.Thu
	 in
	 List.fold_left
	   (fun svg2 event ->
	    let horiz = time_level thu event.from in
	    let block_width = time_level thu event.till - horiz in
	    let args0 =
	      ["x",horiz;
	       "y", vert;
	       "width", block_width;
	       "height", week_height]
	    in
	    let args =
	      add_color event.precision
	      :: List.map (fun (s,i) -> s, string_of_int i) args0
	    in
	    let rect = tag "rect" ~args in
	    let args1 =
	      ["x", string_of_int (horiz + 1);
	       "y", string_of_int (vert + week_height - 2);
	       "fill", "white";
	       "font-size","10";
(*"textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacing"*)]
	    in
	    let content1 =
	      data_elt event.precision
	    in
	    let text1 = tag "text" ~args:args1 ~content:content1
	    in
	    (* let args2 =
	    ["x", string_of_int (horiz + left_padding);
	     "y", string_of_int (vert + 2*block_height / 3 + hour_alignment);
	     "fill", "white";
	     "font-size","14";
	     "textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacingAndGlyphs"]
	  in
	  let text2 = tag "text" ~args:args2 ~content:(data_elt event.location)
	  in*)
	    rect::text1(*::text2*)::svg2
	   )
	   svg agenda.events
       else svg
      )
      [] formations
  in
  let vacancy_blocks =
    let make_block w =
      let args = ["x",string_of_int margin;
		  "y", string_of_int (header_height + week_height * (w-1));
		  "width", string_of_int (2*day_width);
		  "height", string_of_int (2*week_height);
		  "style","fill:rgb(192,192,192); fill-opacity:0.75"]
      in
      tag "rect" ~args
    in
    List.map make_block [9; 17]
  in
  (*let text_date =
    Date.(let this_monday = prev ~d:Mon (next (today ())) in
	  let dateref = match s with
	    | ["graph_current"] -> this_monday
	    | ["graph_coming"] -> next_week this_monday
	    | ["graph_next"] -> next_week (next_week this_monday)
	    | _ -> invalid_arg "Html_timetable.graph"
	  in "Semaine du "^(format_t dateref))
  in
  let args = ["x","0";"y","12";"fill","blue"]
  and content = data_elt text_date in*)
  let svg_code =
    (*(tag "text" ~args ~content)
      ::*) List.flatten [main_lines;
			 second_lines;
			 col_titles;
			 line_titles;
			 blocks;
			 vacancy_blocks
			]
  in
  [tag "svg" ~args:["width",string_of_int width;
		    "height",string_of_int height]
       ~content:svg_code]
