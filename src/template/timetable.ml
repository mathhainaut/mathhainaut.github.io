let year1 = 2017
let year2 = 2018

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

type time = Date.t * Date.period

let single md ?(y=year1) period =
  Date.(make (md, y), to_p period)

let set recur ~from ~till ?(except=[]) period =
  let from = Date.make from and till = Date.make till in
  let f = of_rec recur in
  let rec create exns list date =
    if (Date.diff_days date till < 0) then
      List.rev_map (fun d -> d, Date.to_p period) list
    else
      let jump, exns' =
        match exns with
          [] -> false, []
        | x::l ->
             x = date, l
      in
      create (if jump then exns' else exns)
        (if jump then list else date::list) (f date)
  in
  let start_date =
    if guaranteed_start recur then from
    else f (Date.prev from)
  in create (List.map Date.make except) [] start_date

type t =
{ewhat: string;
ewhere: string;
ewhen: time list}

let make ewhat ewhere ewhen = {ewhat; ewhere; ewhen}

let cp = make "CP"
let cag = make "C Ag"
let st = make "Stage"

let compare (d1,t1,_,_) (d2,t2,_,_) =
  let n = Date.diff_days d1 d2 in
  if n <> 0 then n
  else Date.diff_min (fst t1) (fst t2)

let list_merge lists =
  let n = List.length lists in
  let a = Array.init n (fun i ->
                          List.sort (fun x y -> compare y x) (List.nth lists i))
  in
  let rec constructor date current list =
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
        (date,current)::list
    | Some x ->
        assert (!imin >= 0);
        a.(!imin) <- !lmin;
        let (d,p,q,o) = x in
        if (d = date || (current = [] && list = [])) then
          constructor d ((p,q,o)::current) list
        else
          constructor d [(p,q,o)] ((date, current)::list)
  in constructor (Date.make (Date.Jan 1, 2017)) [] []

(*let mk_fresnes d h ?(jun=false)?(c=126) n s =
  let min = if h > 12 then 30 else 0 in
  let time = Printf.sprintf "%02d:%02d-%02d:%02d" h min (h+1) min in
  let line = Printf.sprintf "Cours (%de %d; A%03d)" n s c in
  make line "Fresnes-sur-Escaut" Date.([single (if jun then Jun d else May d) time]) *)

let periods =
  Date.([(Sep 8, 2017), (Oct 20, 2017);
   (Nov 6, 2017), (Dec 22, 2017);
   (Jan 8, 2018), (Feb 19, 2018);
   (Mar 5, 2018), (Apr 20, 2018);
   (May 7, 2018), (Jul 6, 2018)])

let labbe_time =  function
    8, Start -> "08:05"
  | 8, End
  | 9, Start -> "09:00"
  | 9, End -> "09:55"
  | 10, Start -> "10:10"
  | 10, End
  | 11, Start -> "11:05"
  | 11, End
  | 12, Start -> "12:00"
  | 12, End
  | 13, Start -> "12:55"
  | 13, End -> "13:50"
  | 14, Start -> "13:55"
  | 14, End
  | 15, Start -> "14:50"
  | 15, End -> "15:45"
  | 16, Start -> "16:00"
  | 16, End
  | 17, Start -> "16:55"
  | 17, End -> "17:50"
  | t, _ -> invalid_arg (string_of_int t)

let labbe_1 d h =
  let time =
    Printf.sprintf "%s-%s" (labbe_time (h, Start)) (labbe_time (h, End))
  in
  let merger l (from, till) =
    List.rev_append (Date.(set (Every_ d) ~from ~till time)) l
  in
  List.fold_left merger [] periods

let labbe classe list =
  let rec aux res = function
      [] -> res
    | (d, h) :: l ->
       let list = labbe_1 d h in
       aux (List.rev_append list res) l
  in
  let line = Printf.sprintf "Cours (%s)" classe in
  make line "Douai" (aux [] list)
   
let form ue dates =
  make (Printf.sprintf "Formation (%s)" ue) "Valenciennes" dates

(*let jun = true*)

let t =
  let l = List.map (fun event ->
                    (List.map (fun elt ->
                               let (d,p) = elt in
                               d, p,
                               event.ewhat, event.ewhere)
                              event.ewhen))
		   [
		     (*(make "Non dispo" "Fresnes-sur-Escaut"
			 Date.([single (May 9) "08:30-11:00"]);
		    mk_fresnes 9 11 ~c:118 5 1;
		    mk_fresnes 10 10 3 1;
		    mk_fresnes 10 11 6 1;
		    mk_fresnes 11 10 6 1;
		    mk_fresnes 11 11 6 2;
		    mk_fresnes 12 8 3 1;
		    mk_fresnes 12 9 6 2;
		    mk_fresnes 15 8 6 1;
		    mk_fresnes 15 10 6 2;
		    mk_fresnes 15 11 6 2;
		    mk_fresnes 15 13 3 3;
		    mk_fresnes 15 14 3 1;
		    mk_fresnes 16 10 5 3 ~c:118;
		    mk_fresnes 16 11 3 1;
		    mk_fresnes 17 9 5 1 ~c:118;
		    mk_fresnes 17 10 3 1;
		    mk_fresnes 17 11 6 1;
		    mk_fresnes 18 10 6 1;
		    mk_fresnes 18 11 6 2;
		    mk_fresnes 19 8 3 1 ~c:218;
		    mk_fresnes 19 9 6 2;
		    mk_fresnes 19 10 6 1;
		    mk_fresnes 22 8 6 1;
		    mk_fresnes 22 10 6 2;
		    mk_fresnes 22 11 6 2;
		    mk_fresnes 22 13 3 3;
		    mk_fresnes 22 14 3 1;
		    mk_fresnes 22 15 6 1;
		    mk_fresnes 23 11 5 1 ~c:118;
		    mk_fresnes 23 10 6 2;
		    mk_fresnes 24 10 3 1;
		    mk_fresnes 24 11 6 1;
		    mk_fresnes 29 8 6 1;
		    mk_fresnes 29 11 6 2;
		    mk_fresnes 29 13 3 3 ~c:218;
		    mk_fresnes 29 14 3 1;
		    mk_fresnes 29 15 6 2;
		    mk_fresnes 30 10 5 3 ~c:118;
		    mk_fresnes 30 11 3 1;
		    mk_fresnes 31 9 5 1 ~c:118;
		    mk_fresnes 31 10 3 1;
		    mk_fresnes 31 11 6 1;
		    mk_fresnes ~jun 1 10 6 1;
		    mk_fresnes ~jun 1 11 6 2;
		    mk_fresnes ~jun 2 8 3 1;
		    mk_fresnes ~jun 2 9 6 2;
		    mk_fresnes ~jun 2 10 6 1;
		    mk_fresnes ~jun 6 10 6 2;
		    mk_fresnes ~jun 6 11 5 2 ~c:118;
		    mk_fresnes ~jun 7 10 3 1;
		    mk_fresnes ~jun 7 11 6 1;
		    mk_fresnes ~jun 8 10 6 1;
		    mk_fresnes ~jun 8 11 6 2;
		    mk_fresnes ~jun 9 8 3 1;
		    mk_fresnes ~jun 9 9 6 2;
		    mk_fresnes ~jun 9 10 6 1;
		    mk_fresnes ~jun 12 9 6 1;
		    mk_fresnes ~jun 12 11 6 2;
		    mk_fresnes ~jun 12 13 3 3 ~c:218;
		    mk_fresnes ~jun 12 14 3 1;
		    mk_fresnes ~jun 12 15 6 2;
		    mk_fresnes ~jun 13 10 5 3 ~c:118;
		    mk_fresnes ~jun 13 11 3 0 ~c:125;
		    mk_fresnes ~jun 14 9 5 1 ~c:118;
		    mk_fresnes ~jun 14 10 3 1;
		    mk_fresnes ~jun 14 11 6 1;
		    ( *mk_fresnes ~jun 15 10 6 1;
		    mk_fresnes ~jun 15 11 6 2;
		    mk_fresnes ~jun 16 8 3 1;
		    mk_fresnes ~jun 16 9 6 2;
		    mk_fresnes ~jun 16 10 6 1;* )
		    mk_fresnes ~jun 19 8 6 1;
		    mk_fresnes ~jun 19 10 6 2;
		    mk_fresnes ~jun 19 11 6 2;
		    mk_fresnes ~jun 19 13 3 3 ~c:218;
		    mk_fresnes ~jun 19 14 3 1;
		  ( *  mk_fresnes ~jun 20 10 6 2;
		    mk_fresnes ~jun 20 11 5 2 ~c:118;
		    mk_fresnes ~jun 21 10 3 1;
		    mk_fresnes ~jun 21 11 6 1;
		    mk_fresnes ~jun 22 10 6 1;
		    mk_fresnes ~jun 22 11 6 2;
		    mk_fresnes ~jun 23 8 3 1;
		    mk_fresnes ~jun 23 9 6 2;
		    mk_fresnes ~jun 23 10 6 1;* ) *)
		     cag "Valenciennes" [];

		     
		     cp "Fresnes-sur-Escaut"
			Date.(set (Every_ Sat)
				  (Sep 16, 2017)
				  (Jun 15, 2018) "09:00-11:00");
		     cp "Flines-lez-Râches"
			Date.(single (Oct 7) "13:30-15:00"
			      :: set (Every_ Fri)
				  ~from:(Sep 29, 2017)
				  ~till:(Jun 15, 2018)
				  ~except:[Oct 6, 2017] "17:30-19:00");
		     cp "Aix-lez-Orchies"
			Date.(single (Sep 18) "18:30-20:00"
			      :: single (Sep 23) "11:30-13:00"
			      :: set (Every_ Tue)
				     (Oct 1, 2017)
				     (Jun 15, 2018) "17:30-19:00");
		     
		     labbe "2 6" Date.([Mon, 15; Tue, 10; Fri, 11]);
		     labbe "1 STMG1" Date.([Mon, 14; Tue, 9; Fri, 10]);
		     labbe "2 6 - 2" Date.([Mon, 8]);
		     labbe "2 6 - 1" Date.([Mon, 9]);

		     make "Réunion" "Douai"
			  Date.(set (Every_ Mon)
				    (Sep 18, 2017)
				    (Jun 1, 2018) "16:00-18:00"
				@ set (Every_ Tue)
				      ~from:(Sep 19, 2017)
				      ~till:(Jun 2, 2018)
				      ~except:[Sep 26, 2017] "13:00-15:00");
		     form "LV"
			  Date.(set Every_2weeks
				    (Sep 13, 2017)
				    (Oct 11, 2017) "10:00-12:00");
		     form "TICE"
			  Date.([single (Sep 20) "13:30-14:30";
				 single (Nov 29) "13:30-15:30"]);
		     form "HisMaths"
			  Date.(single (Nov 8) "10:00-12:00"
				:: single (Nov 15) "10:00-12:00"
				:: set Every_2weeks
				    ~from: (Sep 20, 2017)
				    ~till: (Nov 29, 2017)
				    ~except:[Nov 1, 2017;
					     Nov 15, 2017] "10:00-12:00");
		     form "Didac"
			  Date.(single (Sep 14) "09:00-12:00"
				:: single (Oct 5) "09:00-12:00"
				:: single (Nov 23) "09:00-12:00"
				:: single (Dec 7) "10:00-12:00"
				:: single (Dec 13) "10:00-12:00"
				:: []);
		     
		     form "Rech"
			  Date.([single (Sep 13) "13:30-14:00"]);

		     form "ContEx"
			  (List.map (fun d -> single d "09:00-16:00")
				    Date.([Sep 7;
					   Sep 21;
					   Oct 12;
					   Nov 16;
					   Nov 30;
					   Dec 21]));
		     form "SitPro"
			   (List.map (fun d -> single d "13:30-16:00")
				    Date.([Sep 6;
					   Oct 18;
					   Nov 15;
					   Dec 13]));
		   ]
  in
  list_merge l
