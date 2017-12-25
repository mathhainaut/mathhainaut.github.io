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

let gather_data s dateref =
  let cmp, to_register =
    match s with (* Beware on reading! *)
    | ["done"] -> (fun d ->  d <= 0), Some false
    | ["to_register"] -> (fun d ->  d <= 0), Some true
    | ["next"] -> (fun d ->  d > 14), None
    | ["coming"] -> (fun d -> (d >= 0 && d <= 14)), None
    | ["graph_current"] -> (fun d -> (d >= 0 && d < 7)), None
    | ["graph_coming"] -> (fun d -> (d >= 7 && d < 14)), None
    | ["graph_next"] -> (fun d -> (d >= 14 && d < 21)), None
    | _ -> invalid_arg "Html_timetable.gather_data"
  in
  let filterer agenda =
    cmp (Date.diff_days dateref agenda.date)
  in
  List.filter filterer Timetable.t, to_register

let data_elt s =
  let s' = Netconversion.convert
             ~in_enc:(
               Netconversion.encoding_of_string  "iso-8859-1")
             ~out_enc:(
               Netconversion.encoding_of_string  "utf8")
	     s
  in
  [Nethtml.Data s']

let tag ?(args=[]) ?(content=[]) tag = Nethtml.Element(tag,args,content)

let timetable _ s =
  let list, to_register = gather_data s (Date.today ())
  in
  let all_html =
    List.fold_left
      (fun html agenda ->
       let html_date = data_elt (Date.format_t agenda.date)
       in
       let html_events =
         List.fold_left
           (fun html2 event ->
	    let add_line = match to_register with
		None -> true
	      | Some registration_needed ->
		 (registration_needed && not event.registered)
		 || (event.registered && not registration_needed)
	    in
	    if add_line then
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
	    else html2)
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
      [] list
  in
  [tag "ul" ~content:all_html]

let graph _ s =
  let weekdays = ["Lundi";"Mardi";"Mercredi";"Jeudi";"Vendredi";"Samedi";"Dimanche"] in
  let width = 640 and height = 600 in
  let hour_height = 35
  and header_height = 50
  and top_margin = 30 in
  let day_width = 86
  and margin = 38 in
  let left_padding = 10
  and header_position = 40
  and hour_label_position = 0
  and hour_alignment = 5 in
  let hour_start = 7
  and hour_end = 22
  in
  let hours_extent = hour_end - hour_start + 1 in
  let lines =
    let desc =
      let rec f i l =
	if i > 7 + hours_extent then l
	else f (i+1)
	       (if i >= 8 then
		  (true, header_height + hour_height*(i-8)) :: l
		else
		  (false, margin + i * day_width)::l)
      in f 0 []
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
  let col_titles =
    List.mapi (fun i d ->
	       let args =
		 ["x", string_of_int (margin+day_width*i+left_padding);
		  "y", string_of_int header_position;
		  "textLength",string_of_int (day_width- 2*left_padding);]
	       in
	       tag "text" ~args ~content:(data_elt d)) weekdays
  in
  let line_titles =
    let elt_hour i =
      let level = header_height + hour_height*i + hour_alignment in
      let args =
	["x", string_of_int hour_label_position;
	 "y", string_of_int level;
	 "font-size","12"]
      in
      let s = Printf.sprintf "%02d:00" (i+hour_start) in
      tag "text" ~args ~content:(data_elt s)
    in
    Array.to_list (Array.init hours_extent elt_hour)
  in
  let blocks =
    let time_level time =
      let h = time.Date.h in
      if h < 7 || h > 22 then
	let s = string_of_int h in
	invalid_arg ("Html_timetable.graph: time_level hour="^s)
      else
	header_height + hour_height*(h - hour_start)
	+ (hour_height*time.Date.m)/60
    in
    List.fold_left
      (fun svg agenda ->
       let horiz =
	 let k = (Date.weekday_num agenda.date - 1) mod 7
	 in
	 margin + k*day_width
       in
       List.fold_left
	 (fun svg2 event ->
	  let vert = time_level event.from in
	  let block_height = time_level event.till - vert in
	  let args0 =
	    ["x",horiz;
	     "y", vert;
	     "width", day_width;
	     "height", block_height]
	  in
	  let args =
	    ("style", "fill:blue; fill-opacity:0.5")
	    :: List.map (fun (s,i) -> s, string_of_int i) args0
	  in
	  let rect = tag "rect" ~args in
	  let args1 =
	    ["x", string_of_int (horiz + left_padding);
	     "y", string_of_int (vert + block_height / 3 + hour_alignment);
	     "fill", "white";
	     "font-size","10";
	     (*"textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacing"*)]
	  in
	  let content1 =
	    data_elt (match event.precision with
			"" -> event.data
		      | s -> event.data^" ("^s^")")
	  in
	  let text1 = tag "text" ~args:args1 ~content:content1
	  in
	  let args2 =
	    ["x", string_of_int (horiz + left_padding);
	     "y", string_of_int (vert + 2*block_height / 3 + hour_alignment);
	     "fill", "white";
	     "font-size","14";
	     "textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacingAndGlyphs"]
	  in
	  let text2 = tag "text" ~args:args2 ~content:(data_elt event.location)
	  in
	  rect::text1::text2::svg2
	 )
	 svg agenda.events
      )
      [] (let this_monday =
	    Date.(prev ~d:Mon (next (today ())))
	  in
	  let list, _ = gather_data s this_monday
	  in list)
  in
  let text_date =
    Date.(let this_monday = prev ~d:Mon (next (today ())) in
	  let dateref = match s with
	    | ["graph_current"] -> this_monday
	    | ["graph_coming"] -> next_week this_monday
	    | ["graph_next"] -> next_week (next_week this_monday)
	    | _ -> invalid_arg "Html_timetable.graph"
	  in "Semaine du "^(format_t dateref))
  in
  let args = ["x","0";"y","12";"fill","blue"]
  and content = data_elt text_date in
  let svg_code =
    (tag "text" ~args ~content)
    :: List.flatten [lines;
		     col_titles;
		     line_titles;
		     blocks
		    ]
  in
  [tag "svg" ~args:["width",string_of_int width;
		   "height",string_of_int height]
      ~content:svg_code]
