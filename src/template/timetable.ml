let year1 = 2017
let year2 = 2017

type recurrence =
    Every_day
  | Every_ of Date.day
  | Every_month
  | Every_2weeks
  | Every_1st of Date.day

let of_rec = function
    Every_day -> Date.next ?d:None
  | Every_(d) -> Date.next ~d
  | Every_month -> Date.next_m  ?d:None
  | Every_2weeks -> fun date -> Date.next_week (Date.next_week date)
  | Every_1st(d) -> Date.next_m ~d


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
  in create (List.map Date.make except) [] (f (Date.prev from))

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

let mk_fresnes d h ?(jun=false)?(c=126) n s =
  let min = if h > 12 then 30 else 0 in
  let time = Printf.sprintf "%02d:%02d-%02d:%02d" h min (h+1) min in
  let line = Printf.sprintf "Cours (%de %d; A%03d)" n s c in
  make line "Fresnes-sur-Escaut" Date.([single (if jun then Jun d else May d) time])

let jun = true

let t =
  let l = List.map (fun event ->
                    (List.map (fun elt ->
                               let (d,p) = elt in
                               d, p,
                               event.ewhat, event.ewhere)
                              event.ewhen))
		   [make "Non dispo" "Fresnes-sur-Escaut"
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

		    cp "Nomain"
		       Date.(single (Jun 8) "16:00-17:00"
			     :: single (Jun 8) "17:00-18:00"
			     :: single (Jun 9) "12:00-13:00"
			     :: set (Every_ Mon)
				    ~from:(May 22, 2017)
				    ~till:(Jun 19, 2017)
				    ~except:[Jun 5, 2017] "18:00-19:00");
		    cag "Valenciennes" [];
		    cp "Fresnes-sur-Escaut"
		       Date.(set (Every_ Sat)
				 ~from:(May 20, 2017)
				 ~till:(Jun 3, 2017) "08:30-10:30");
		    cp "Faumont"
		       Date.(set (Every_ Sat)
				 ~from: (May 20, 2017)
				 ~till: (May 27, 2017) "11:15-13:15");
		    cp "Flines-lez-Râches"
		       Date.(single (May 26) "09:00-11:00"
			     :: single (Jun 6) "14:00-16:00"
			     :: set (Every_ Sat)
				     ~from: (May 20, 2017)
				     ~till: (Jun 17, 2017)
				     ~except:[May 27, 2017] "13:30-15:30");
		    cp "Coutiches"
		       Date.(single (May 26) "11:15-12:45"
			     :: set (Every_ Sat)
				   ~from: (Jun 3, 2017)
				   ~till: (Jun 17, 2017) "11:15-12:45");
		    cp "Saméon"
		       Date.([]);
		    cp "Nivelle"
		       Date.([]);
		    make "Concours CAPES" "Nancy"
			 Date.(single (Jun 15) "00:00-23:59"
			       :: single (Jun 16) "00:00-23:59"
			       :: []);
		    make "Concours Agreg" "Lille"
			 Date.(single (Jun 22) "12:00-19:00"
			       :: single (Jun 23) "08:00-19:00"
			       :: single (Jun 24) "08:00-19:00"
			       :: [])
		   ]
  in
  list_merge l
