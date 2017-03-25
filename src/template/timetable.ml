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
       Date.(set (Every_ Mon)
               ~from:(Mar 20, 2017)
               ~till:(Jun 19, 2017)
               ~except:[May 1, 2017] "18:00-19:00");
     cp "Auchy-lez-Orchies"
       Date.(set (Every_ Wed)
               ~from:(Mar 22, 2017)
               ~till:(Jun 21, 2017) "16:30-18:00");
     cag "Valenciennes"
       Date.(set (Every_ Thu)
               ~from:(Mar 23, 2017)
               ~till:(Jul 6, 2017)
               ~except:[Apr 13, 2017;
                        Apr 20, 2017;
                        May 25, 2017] "18:00-20:00");
     cag "Valenciennes"
       Date.(set (Every_ Fri)
               ~from:(Mar 24, 2017)
               ~till:(Jul 7, 2017)
               ~except:[Apr 14, 2017;
                        Apr 21, 2017] "17:30-19:30");
     cp "Fresnes-sur-Escaut"
       Date.(set (Every_ Sat)
               ~from:(Mar 25, 2017)
               ~till:(Jun 3, 2017)
               ~except:[Apr 8, 2017;
                        Apr 15, 2017] "08:30-10:30");
     cp "Faumont"
       Date.(set (Every_1st Sat)
               ~from: (Apr 1, 2017)
               ~till: (Jun 3, 2017) "11:15-12:15"
             @
               set (Every_ Sat)
               ~from: (Mar 25, 2017)
               ~till: (Jun 30, 2017)
               ~except:[Apr 1, 2017;
                        May 6, 2017;
                        Jun 3, 2017] "11:15-13:15");
    cp "Flines-lez-Râches"
      Date.(set (Every_ Sat)
              ~from: (Mar 25, 2017)
              ~till: (Jun 30, 2017) "13:30-15:30");
    cp "Coutiches"
      Date.(set (Every_ Sat)
              ~from: (Mar 25, 2017)
              ~till: (Jun 30, 2017)
              ~except: [Apr 1, 2017] "15:45-17:15"
            @
           [single (Mar 31) "15:00-16:30"]);
    cp "Saméon"
      Date.(single (Mar 21) "18:00-20:00" ::
              set (Every_ Sat)
              ~from: (Mar 25, 2017)
              ~till: (Jun 30, 2017)
              ~except:[Mar 25, 2017] "17:45-19:45");
    make "Concours Agreg" "Douai" Date.([single (Mar 23) "09:00-15:00"]);
    make "Concours Capes" "Douai" Date.([single (Apr 3) "09:00-14:00";
                                         single (Apr 4) "09:00-14:00"]);
    cp "Nomain" Date.([single (Mar 23) "16:15-17:15"]);
    make "Non dispo" "Saint-Amand-les-Eaux" Date.([single (Apr 3) "16:00-17:30"]);
    make "Non dispo" "Valenciennes" Date.([single (Apr 6) "13:45-14:30"]);
]
in
list_merge l
