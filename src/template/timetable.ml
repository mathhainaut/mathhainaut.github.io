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
  in create (List.map Date.make except) [] from

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

let t =
  let l = List.map (fun event ->
                      (List.map (fun elt ->
                                   let (d,p) = elt in
                                   d, p,
                                   event.ewhat, event.ewhere)
                         event.ewhen))
    [cp "Nomain"
       Date.(single (Apr 20) "09:00-10:00"::
               single (Apr 21) "11:15-12:15"::
               set (Every_ Mon)
               ~from:(Apr 10, 2017)
               ~till:(Jun 19, 2017)
               ~except:[Apr 10, 2017;
                        Apr 17, 2017;
                        May 1, 2017] "18:00-19:00");
     cag "Valenciennes"
       Date.(set (Every_ Thu)
               ~from:(Apr 27, 2017)
               ~till:(Jul 6, 2017)
               ~except:[May 25, 2017] "18:00-20:00");
     cag "Valenciennes"
       Date.(set (Every_ Fri)
               ~from:(Apr 28, 2017)
               ~till:(Jul 7, 2017)
               ~except:[] "17:30-19:30");
     cp "Fresnes-sur-Escaut"
       Date.(set (Every_ Sat)
               ~from:(Apr 8, 2017)
               ~till:(Jun 3, 2017)
               ~except:[Apr 8, 2017;
                        Apr 15, 2017] "08:30-10:30");
     cp "Faumont"
       Date.(set (Every_1st Sat)
               ~from: (May 6, 2017)
               ~till: (Jun 3, 2017) "11:15-12:15"
             @
               set (Every_ Sat)
               ~from: (Apr 22, 2017)
               ~till: (Jun 30, 2017)
               ~except:[May 6, 2017;
                        Jun 3, 2017] "11:15-13:15");
     cp "Flines-lez-Râches"
       Date.(set (Every_ Sat)
               ~from: (Apr 22, 2017)
               ~till: (Jun 30, 2017) "13:30-15:30");
     cp "Coutiches"
       Date.(single (Apr 10) "11:30-13:00" ::
               single (Apr 20) "11:15-12:45"::
               set (Every_ Sat)
               ~from: (Apr 22, 2017)
               ~till: (Jun 30, 2017) "15:45-17:15");
     cp "Saméon"
       Date.([single (Apr 8) "10:30-12:30"]);
     make "CP-Stage" "Vieux-Condé" Date.(set Every_day
                                           ~from: (Apr 10, 2017)
                                           ~till: (Apr 14, 2017) "09:00-11:00");
     make "CP-Stage" "Sars-et-Rosières" Date.([single (Apr 19) "09:00-11:00";
                                               single (Apr 21) "09:00-11:00"]);
    ]
  in
  list_merge l
