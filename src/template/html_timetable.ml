open Nethtml
module H = Nethtml       

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
  let cmp =
    match s with (* Beware on reading! *)
      ["done"] -> (fun d ->  d <= 0)
    | ["next"] -> (fun d ->  d > 14)
    | ["coming"] -> (fun d -> (d >= 0 && d <= 14))
    | ["graph_current"] -> (fun d -> (d >= 0 && d < 7))
    | ["graph_coming"] -> (fun d -> (d >= 7 && d < 14))
    | _ -> invalid_arg "Html_timetable.gather_data"
  in
  let filterer (x,_) = cmp (Date.diff_days dateref x) in
  List.filter filterer Timetable.t

let data_elt s =
  let s' = Netconversion.convert
             ~in_enc:(
               Netconversion.encoding_of_string  "iso-8859-1")
             ~out_enc:(
               Netconversion.encoding_of_string  "utf8")
	     s
  in
  [H.Data s']

let timetable _ s =
  let all_html =
    List.fold_left
      (fun html (date, events) ->
         let html_date = H.Data (text_of_date date)
         in
         let html_events =
           List.fold_left
             (fun html2 (period,what,where) ->
                let line =
                  Printf.sprintf "%s-%s : %s, %s"
                    (Date.format_s (fst period)) (Date.format_s (snd period))
                    what where
                in
                let data = data_elt line in
                H.Element("li",[], data)::html2)
             [] events
         in
         let li_args =
           [html_date;
            H.Element("ul",[],html_events)]
         in
         let li = [H.Element("li",[],li_args)] in
         let span_args =
           [
            "style","javascript:setDisp("^(Date.format_t ~sep:"-" date)^")"
           ]
         in
         H.Element("span",span_args, li) :: html)
      [] (gather_data s (Date.today ()))
  in
  [H.Element("ul",[],all_html)]

let graph _ s =
  let weekdays = ["Lundi";"Mardi";"Mercredi";"Jeudi";"Vendredi";"Samedi";"Dimanche"] in
  let width = 820 and height = 650 in
  let hour_height = 40
  and header_height = 40
  and top_margin = 15 in
  let day_width = 110
  and margin = 50 in
  let left_padding = 10
  and header_position = 30
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
		  H.Element("line",args,[])) desc
  in
  let col_titles =
    List.mapi (fun i d ->
	       let args =
		 ["x", string_of_int (margin+day_width*i+left_padding);
		  "y", string_of_int header_position]
	       in
	       H.Element("text", args, [H.Data d])) weekdays
  in
  let line_titles =
    let elt_hour i =
      let level = header_height + hour_height*i + hour_alignment in
      let args =
	["x", string_of_int hour_label_position;
	 "y", string_of_int level]
      in
      let s = Printf.sprintf "%02d:00" (i+hour_start) in
      H.Element("text", args, [H.Data s])
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
    let period_bounds (p1, p2) =
      time_level p1, time_level p2
    in
    List.fold_left
      (fun svg (date, events) ->
       let horiz =
	 let k = Date.(match weekday date with
			 Mon -> 0
		       | Tue -> 1
		       | Wed -> 2
		       | Thu -> 3
		       | Fri -> 4
		       | Sat -> 5
		       | Sun -> 6)
	 in
	 margin + k*day_width
       in
       List.fold_left
	 (fun svg2 (period, what, where) ->
	  let vert = time_level (fst period) in
	  let block_height = time_level (snd period) - vert in
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
	  let rect = H.Element("rect",args,[]) in
	  let args1 =
	    ["x", string_of_int (horiz + left_padding);
	     "y", string_of_int (vert + block_height / 3 + hour_alignment);
	     "fill", "white";
	     "font-size","10";
	     (*"textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacing"*)]
	  in
	  let text1 = H.Element("text",args1,data_elt what)
	  in
	  let args2 =
	    ["x", string_of_int (horiz + left_padding);
	     "y", string_of_int (vert + 2*block_height / 3 + hour_alignment);
	     "fill", "white";
	     "font-size","14";
	     "textLength",string_of_int (day_width- 2*left_padding);
	     "lengthAdjust","spacing"]
	  in
	  let text2 = H.Element("text",args2,data_elt where)
	  in
	  rect::text1::text2::svg2
	 )
	 svg events
      )
      [] (gather_data s Date.(prev ~d:Mon (today ())))
  in
  let text_date =
    Date.(let last_mon = prev ~d:Mon (today ()) in
	  let dateref = match s with
	    | ["graph_current"] -> last_mon
	    | ["graph_coming"] -> next ~d:Mon (next last_mon)
	    | _ -> invalid_arg "Html_timetable.graph"
	  in "Semaine du "^(format_t dateref))
  in
  let svg_code =
    H.Element("text",["x","0";"y","12";"fill","blue"],data_elt text_date)
    :: List.flatten [lines;
		     col_titles;
		     line_titles;
		     blocks
		    ]
  in
  [H.Element("svg",["width",string_of_int width;
		    "height",string_of_int height],svg_code)]
