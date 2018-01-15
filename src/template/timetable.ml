let ref_year = 2017

let days_off =
  Date.([Apr 2; May 8; May 10; May 21])

let periods =
  Date.([Sep 8, Oct 20;
	 Nov 6, Dec 22;
	 Jan 8, Feb 23;
	 Mar 12, Apr 20;
	 May 7, Jul 6])

type recurrence =
    Every_day
  | Every_ of Date.day
  | Every_month
  | Every_2weeks
  | Every_1st of Date.day

type period_bound =
    Start
  | End

let of_rec = function
    Every_day -> Date.next ?d:None
  | Every_(d) -> Date.next ~d
  | Every_month -> Date.next_m  ?d:None
  | Every_2weeks -> fun date -> Date.next_week (Date.next_week date)
  | Every_1st(d) -> Date.next_m ~d

let guaranteed_start = function
    Every_day
  | Every_month
  | Every_2weeks -> true
  | Every_ _
  | Every_1st _ -> false

type time = Date.month * Date.s * Date.s

let mkdate md =
  let open Date in
  let y =
    match md with
      Sep _ | Oct _ | Nov _ | Dec _ -> ref_year
      | _ -> ref_year + 1
  in
  make (md, y)
		       
let ( => ) date_list period =
  List.rev_map (fun d -> d, fst period, snd period) date_list
	       
let from start ~till ?(except=[]) recur =
  let start = mkdate start and till = mkdate till in
  let f = of_rec recur in
  let rec create exns list date =
    if (Date.diff_days date till < 0) then
      list
    else
      let k, l =
	match exns with
          [] -> -1, exns
        | x::l ->
	   Date.diff_days date x, l
      in
      let exns' =
	if k <= 0 then l
	else exns
      in
      let list' =
	if k = 0 then list
	else (fst (Date.get date))::list
      in
      create exns' list' (f date)
  in
  let start_date =
    if guaranteed_start recur then start
    else f (Date.prev start)
  in create (List.map mkdate except) [] start_date

type gen_event =
    {ewhat: string;
     eprecision: string;
     ewhere: string;
     ewhen: time list;
     elastregistered: Date.t option;}
     
let make0 ewhat ?(eprecision="") ewhere ?last_registered ewhen =
  {ewhat; eprecision; ewhere; ewhen = List.flatten ewhen;
   elastregistered=match last_registered with
		     None -> None
		   | Some d -> Some (mkdate d)}

let cp = make0 "CP"
let st = make0 "Stage"

let event = make0 ?last_registered:None

let cpn = event "CP"
let cag = event "C Ag"

type event =
    {from: Date.s;
     till: Date.s;
     data: string;
     precision: string;
     location: string;
     registered: bool;}

type diary =
    {date: Date.t;
     events: event list;}

let compare (d1,ev1) (d2,ev2) =
  let n = Date.diff_days d1 d2 in
  if n <> 0 then n
  else
    let f1 = ev1.from
    and t1 = ev1.till
    and f2 = ev2.from
    and t2 = ev2.till
    in
    let m = Date.diff_min f1 f2 in
    let e1, b2 =
      if m > 0 then t1, f2
      else t2, f1
    in
    if b2 < e1 then
      (let f () s = Date.format_s s in
       let s =
	Printf.sprintf "***Warning: event overlap on %a\n\
			Event 1:%s, %s\nFrom %a till %a \n\
			Event 2:%s, %s\nFrom %a till %a\n"
		       (fun () t -> Date.format_t t) d1
		       ev1.data ev1.location f f1 f t1
		       ev2.data ev2.location f f2 f t2
      in print_string s);
    m
      
let split_elements timetable =
  let split_event event =
    List.map (fun elt ->
              let (d, t1, t2) = elt in
	      let date = mkdate d in
	      let event = {from = t1;
			   till = t2;
			   data = event.ewhat;
			   precision = event.eprecision;
			   location = event.ewhere;
			   registered =
			     match event.elastregistered with
			       None -> true
			     | Some dr -> Date.diff_days date dr >= 0}
	      in
              date, event)
             event.ewhen
  in
  let lists =  List.map split_event timetable
  in
  let n = List.length lists in
  let a = Array.init n (fun i ->
                        List.sort (fun x y -> compare y x) (List.nth lists i))
  in
  let rec constructor date events list =
    let imin = ref (-1)
    and xmin = ref None
    and lmin = ref [] in
    let f i = function
        [] -> ()
      | x::l ->
        let b =
          match !xmin with
            None -> true
          | Some y -> compare x y > 0
        in
        if b then (
          imin := i;
          xmin := Some x;
          lmin := l)
    in
    Array.iteri f a;
    match !xmin with
      None ->
        assert (!imin < 0);
        assert (!lmin = []);
        {date;events}::list
    | Some x ->
        assert (!imin >= 0);
        a.(!imin) <- !lmin;
        let (d,ev) = x in
        if (d = date || (events = [] && list = [])) then
          constructor d (ev::events) list
        else
          constructor d [ev] ({date; events}::list)
  in constructor Date.(make (Jan 1, ref_year)) [] []
	 
let labbe_time =
  let open Date in
  function
    8, Start -> 08|:05
  | 8, End
  | 9, Start -> 09|:00
  | 9, End -> 09|:55
  | 10, Start -> 10|:10
  | 10, End
  | 11, Start -> 11|:05
  | 11, End
  | 12, Start -> 12|:00
  | 12, End
  | 13, Start -> 12|:55
  | 13, End -> 13|:50
  | 14, Start -> 13|:55
  | 14, End
  | 15, Start -> 14|:50
  | 15, End -> 15|:45
  | 16, Start -> 16|:00
  | 16, End
  | 17, Start -> 16|:55
  | 17, End -> 17|:50
  | t, _ -> invalid_arg (string_of_int t)

let labbe_0 d ?(filter=fun _ -> true) time =
  let merger l (start, till) =
    let dates = from start ~till ~except:days_off (Every_ d) in
    List.rev_append (dates => time) l
  in
  let full = List.fold_left merger [] periods
  in
  List.filter filter full

let labbe_1 d filter h =
  let time =
    (labbe_time (h, Start)), (labbe_time (h, End))
  in
  labbe_0 d ~filter time

let labbe classe ?(filter=fun _ -> true) list =
  let rec aux res = function
      [] -> res
    | (d, h) :: l ->
       let list = labbe_1 d filter h in
       aux (list::res) l
  in
  event "Cours" ~eprecision:classe "Douai" (aux [] list)
   
let form ue dates =
  event "Formation" ~eprecision:ue "Valenciennes" dates
	     
let cours =
  Date.( split_elements
	   [
	     cag "Valenciennes"  [[Oct 24, 16|:00, 18|:00]];
	     
	     cp "Fresnes-S" ~last_registered:(Dec 25)
		[from (Aug 28) ~till:(Nov 4) (Every_ Sat)
		      ~except:[Oct 14; Nov 11; Nov 18; Nov 25]
		 (* Intentionally further than till date, in
		      order to count removed lessons *)
		 => (09|:00, 11|:00);
		 from (Dec 2) ~till:(Jun 15) (Every_ Sat)
		      ~except:[Dec 30; Jan 6; Jan 13; Jan 20; Feb 3]
		 => (08|:30, 10|:30);
		 [Nov 25, 13|:00, 15|:00];
		 [Jan 20, 09|:00, 11|:00];
		 [Dec 28; Jan 4]
		 => (14|:00, 16|:00)
		];

	     cp "Fresnes-STL" ~last_registered:(Dec 25)
		[from (Dec 2) ~till:(Jun 15) (Every_ Sat)
		      ~except:[Dec 30; Jan 6; Jan 13; Jan 20; Feb 3]
		 => (10|:30, 12|:30);
		 [Nov 25, 15|:00, 17|:00];
		 [Dec 28; Jan 4]
		 => (16|:00, 18|:00)
		];

	     cp "Sam�on" ~last_registered:(Dec 25)
		[from (Nov 16) ~till:(Jun 12)  (Every_ Wed)
		      ~except:[Dec 6; Dec 13; Dec 20; Dec 27;
			       Jan 3; Jan 10]  
		 => (17|:15, 18|:45);
		 [Jan 10, 18|:00, 19|:00;
		  Jan 13, 13|:15, 13|:45];
		 [Dec 29; Jan 2; Jan 3; Jan 6]
		 => (10|:30, 12|:00)
		];

	     cp "Flines-lez-R�ches" ~last_registered:(Dec 25)
		[from (Sep 29) ~till:(Jun 15)  (Every_ Fri)
		      ~except:[Oct 6; Oct 27; Nov 3; Nov 10; Nov 17; Dec 29;
			       Jan 5]
		 => (17|:30, 19|:00);
		 [Oct 7; Oct 24]
		 => (13|:30, 15|:00)
		];

	     cp "Aix-lez-Orchies" ~last_registered:(Dec 25)
		[from (Oct 1) ~till:(Jun 15) (Every_ Tue)
		      ~except:[Oct 24; Oct 31; Nov 14; Dec 19; Dec 26;
			       Jan 2]
		 => (17|:30, 19|:00);
		 [Sep 18, 18|:30, 20|:00;
		  Sep 23, 11|:30, 13|:00;
		  Oct 24, 09|:00, 10|:30]
		];

	     cp "Saint-Amand" ~last_registered:(Dec 25)
		[[Nov 8; Nov 22]
		 => (13|:15, 14|:15)];

	     cp "Valenciennes" ~last_registered:(Dec 1)
		[[Dec 15, 20|:15, 21|:15;
		  Dec 17, 18|:00, 20|:00]];

	     cpn "Nomain"
		 [[Dec 9, 13|:15, 15|:15;
		   Dec 16, 13|:15, 14|:15];
		  [Jan 2; Jan 3; Jan 6]
		  => (08|:30, 10|:00)]]
       )
let enseignement =
  Date.( split_elements
	   [
	     labbe "2 6" [Mon, 15; Tue, 10; Fri, 11];
	     labbe "1 STMG1" [Mon, 14; Tue, 9; Fri, 10];
	     labbe "2 6 - 2" [Mon, 8];
	     labbe "2 6 - 1" [Mon, 9];

	     event "R�union" "Douai"
		   (let reun_mon =
		      let filter (d,_,_) =
			match d with
			  Sep n -> n >= 18
			| Dec n -> n <> 4
			| Jan n -> n <> 15
			| _ -> true
		      in 
		      labbe_0 Mon ~filter (16|:55, 18|:00)
		    and reun_tue =
		      let filter (d,_,_) =
			match d with
			  Sep n -> n = 19
			| Oct n -> n >= 10
			| _ -> true
		      in 
		      labbe_0 Tue ~filter (14|:00, 15|:00)
		    and reun_fri =
		      let filter (d,_,_) =
			match d with
			  Sep _ -> false
			| Oct n -> n >= 13
			| _ -> true
		      in
		      labbe_0 Fri ~filter (15|:00, 16|:00)
		    in
		    [[Oct 6, 15|:00, 16|:00;
		      Nov 18, 08|:30, 12|:00;
		      Jan 13, 08|:30, 12|:00];
		     reun_mon;
		     reun_tue;
		     reun_fri]
		   );
	     event "CC" "Douai"
		   [[Nov 30, 16|:55, 18|:15;
		     Dec 6, 16|:00, 17|:30]];
	     
	     event "R�union PP" "Douai"
		   [[Dec 19, 18|:00, 21|:00]];
	   ])

let formations =
  Date.( split_elements
	   [
	     form "LV"
		  [from (Sep 13) ~till:(Oct 11) Every_2weeks
		   => (10|:00, 12|:00);
		   [Jan 25; Feb 15; Mar 15]
		   => (09|:30, 11|:30)
		  ];

	     form "TICE"
		  [[Sep 20, 13|:30, 14|:30;
		    Nov 29, 13|:30, 15|:30;
		    Feb 21, 13|:30, 16|:30]];

	     form "HisMaths"
		  [[Nov 8, 09|:00, 11|:00;
		    Nov 22, 10|:00, 12|:00];
		   List.append
		     (from (Sep 20) ~till:(Nov 29) Every_2weeks
			   ~except:[Nov 1; Nov 15])
		     (from (Jan 31) ~till:(Mar 28) Every_2weeks
			   ~except:[Feb 28])
		   => (10|:00, 12|:00);
		   [Jan 24; Apr 4]
		   => (10|:00, 12|:00);
		  ];

	     form "Didac"
		  [[Sep 14; Oct 5; Nov 23]
		   => (09|:00, 12|:00);
		   [Dec 7; Dec 13]
		   => (10|:00, 12|:00);
		   [Jan 10; Jan 17; Feb 7]
		   => (09|:00, 11|:30);
		   [Feb 21; Mar 21]
		   => (09|:30, 11|:30)
		  ];
	     
	     form "Rech"
		  [[Sep 13, 13|:30, 14|:00]];

	     form "ContEx"
		  [[Sep 7; Sep 21; Oct 12; Nov 16; Nov 30; Dec 21;
		    Jan 11; Feb 22; Mar 15; Mar 29; Apr 19]
		   => (09|:00, 16|:00)];
	     
	     event "Formation" ~eprecision:"Immersion" "Douai"
		   [[Jan 18; Feb 1]
		    => (09|:00, 16|:00)];
	     
	     form "SitPro"
		  [[Sep 6; Oct 18; Nov 15; Dec 13;
		    Jan 17; Jan 31; Mar 14; Mar 28]
		   => (13|:30, 16|:00)];
	   ])

let divers =
  Date.( split_elements
	   [
	     event "Non dispo" "" [[Sep 26, 12|:00, 22|:00]];

	     event "Agreg" "Douai, Lille"
		   [[Mar 22, 09|:00, 15|:00]]
	   ])

let t =
  List.flatten [cours; enseignement; formations; divers]
