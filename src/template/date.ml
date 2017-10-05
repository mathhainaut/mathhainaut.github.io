type t = {day:int;month:int;year:int}

type month =
  | Jan of int
  | Feb of int
  | Mar of int
  | Apr of int
  | May of int
  | Jun of int
  | Jul of int
  | Aug of int
  | Sep of int
  | Oct of int
  | Nov of int
  | Dec of int

let bissex y =
  if y < 1582 then y mod 4 = 0
  else y mod 400 = 0 || (y mod 4 = 0 && y mod 100 <> 0)

let month_length y =
  function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if bissex y then 29 else 28
  | i -> invalid_arg ("month_length: "^(string_of_int i))

let make (md, y) =
  let m, d = match md with
    | Jan n -> 1, n
    | Feb n -> 2, n
    | Mar n -> 3, n
    | Apr n -> 4, n
    | May n -> 5, n
    | Jun n -> 6, n
    | Jul n -> 7, n
    | Aug n -> 8, n
    | Sep n -> 9, n
    | Oct n ->10, n
    | Nov n ->11, n
    | Dec n ->12, n
  in
  if (month_length y m) < d then
    let date = (string_of_int d)^"/"^(string_of_int m)
    in
    failwith ("Date.make:"^date)
  else
    {day=d;month=m;year=y}

let mk d m y = {day=d;month=m;year=y}

let format t = Printf.sprintf "%02d%02d%04d" t.day t.month t.year
let format_t ?(sep="/") t =
  Printf.sprintf "%02d%s%02d%s%04d" t.day sep t.month sep t.year
let format_ymd t = Printf.sprintf "%04d%02d%02d" t.year t.month t.day

let today () =
  let time = Unix.localtime (Unix.time ()) in
  {day = time.Unix.tm_mday;
   month = time.Unix.tm_mon + 1;
   year = time.Unix.tm_year + 1900}


let next_m1 t =
  if t.month < 12 then
    mk 1 (t.month + 1) t.year
  else mk 1 1 (t.year + 1)

let prev_m1 t =
  if t.month > 1 then
    let m = t.month - 1 in mk (month_length t.year m) m t.year
  else mk 31 12 (t.year - 1)

let next_1 t =
  if t.day < month_length t.year t.month then
    mk (t.day+1) t.month t.year
  else next_m1 t

let prev_1 t =
  if t.day > 1 then mk (t.day-1) t.month t.year
  else prev_m1 t

type s = {h:int;m:int}

let incr_hour ?(midnight=true) hour =
  let h' =  hour.h + 1 in
  let h'' = if midnight then h' mod 24 else h' in
  {h= h''; m = hour.m}

let now () =
  let time = Unix.localtime (Unix.time ()) in
  {h=time.Unix.tm_hour; m=time.Unix.tm_min}

let now_today ?m () =
  let time = Unix.localtime (Unix.time ()) in
  {h=time.Unix.tm_hour; m=(match m with None -> time.Unix.tm_min | Some m -> m)},
  {day = time.Unix.tm_mday;
   month = time.Unix.tm_mon + 1;
   year = time.Unix.tm_year + 1900}

let format_s ?(sep=":") s = Printf.sprintf "%02d%s%02d" s.h sep s.m

let to_s s =
  if String.length s <> 5 then invalid_arg ("to_s:"^s^"-length not equal to 5");
  let n1 = String.sub s 0 2 and n2 = String.sub s 3 2 in
  if s.[2] <> ':' then invalid_arg ("to_s:"^s^"-missing ':' separator")
  else
    try {h = int_of_string n1;
         m = int_of_string n2}
    with Invalid_argument msg ->
      invalid_arg ("to_s:"^s^"-"^msg)

type period = s * s

let to_p s =
  if String.length s <> 11 then invalid_arg ("to_s:"^s^"-length not equal to 11");
  let n1 = String.sub s 0 5 and n2 = String.sub s 6 5 in
  if s.[5] <> '-' then invalid_arg ("to_s:"^s^"-missing '-' separator")
  else
    try to_s n1, to_s n2
   with Invalid_argument msg ->
     invalid_arg ("to_p:"^s^"-"^msg)


let add_min s min =
  let h' = s.h + (min / 60) in
  let m' = s.m + (min mod 60) in
  let m = m' mod 60 and h = (h' + m'/60) in
  {h=h;m=m}

let minutes s = 60 * s.h + s.m

let diff_min h1 h2 = minutes h2 - minutes h1

let diff_days d1 d2 =
  let diff_months d1 d2 =
    assert (d1.year = d2.year);
    let da,db, inv =
      if d1.month < d2.month then  d1, d2, false
      else d2, d1, true
    in
    let rec diff accu mc =
      if mc = db.month then accu
      else diff (accu + month_length db.year mc) (mc + 1)
    in
    let full_months = diff 0 (da.month+1) in
    let diff1 = month_length da.year da.month - da.day in
    let sum = diff1 + full_months + db.day in
    if inv then - sum else sum
  in
  if d1.year = d2.year then
    if d1.month = d2.month then
      d2.day - d1.day
    else
      diff_months d1 d2
  else
    let da, db, inv =
      if d1.year < d2.year then d1, d2, false
      else d2, d1, true
    in
    let rec diff accu yc =
      if yc = db.year then accu
      else diff (accu + (if bissex yc then 366 else 365)) (yc + 1)
    in
    let full_years = diff 0 (da.year + 1) in
    let diff1 =
      if da.month = 12 then 31 - da.day
      else diff_months da (mk 31 12 da.year)
    and diff2 =
      if db.month = 1 then db.day
      else (diff_months (mk 1 1 db.year) db) + 1
        (* otherwise forgets to count 1/1/db.year! *)
    in
    let sum = diff1 + full_years + diff2 in
    if inv then - sum else sum

(* Sunday *)

type day =
  | Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

let of_day = function
    Sun -> 0
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6

let to_day = function
    0 -> Sun
  | 1 -> Mon
  | 2 -> Tue
  | 3 -> Wed
  | 4 -> Thu
  | 5 -> Fri
  | 6 -> Sat
  | _ -> invalid_arg "to_day"

let reference_date = {day=1; month = 1; year = 2012}

let weekday_num date = (diff_days reference_date date) mod 7
let weekday date = to_day (weekday_num date)

let num_is_weekend n = n = 0 || n = 6

let weekend date =
  let n = weekday date in
  n = Sat || n = Sun

let is_between ~d d1 d2 =
  diff_days d d1 > 0 && diff_days d d2 < 0


let fun_opt ~fdef ~frec ?d = match d with
    None -> fdef
  | Some day ->
      let rec loop date =
        if weekday date = day then date
        else loop (frec date)
      in fun date -> loop (fdef date)


let next = fun_opt ~fdef:next_1 ~frec:next_1
let next_m = fun_opt ~fdef:next_m1 ~frec:next_1

let prev = fun_opt ~fdef:prev_1 ~frec:prev_1
let prev_m = fun_opt ~fdef:prev_m1 ~frec:prev_1

let loop7 frec =
  let rec loop date = function
    0 -> date
  | n -> loop (frec date) (n-1)
  in fun date -> loop date 7

let next_week = loop7 next
let prev_week = loop7 prev
