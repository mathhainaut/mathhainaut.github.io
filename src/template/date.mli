(**{2 Dates}*)
type t
type month =
    Jan of int
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

val bissex : int -> bool
val month_length : int -> int -> int
val make : month * int -> t

val format : t -> string
val format_t : t -> string
val format_ymd : t -> string

val today : unit -> t

(**{2 Dates and days}*)

type day =
  | Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

val weekday : t -> day
val weekend : t -> bool
val next : ?d:day -> t -> t
val prev : ?d:day -> t -> t
val next_m : ?d:day -> t -> t
val prev_m : ?d:day -> t -> t
val next_week: t -> t
val prev_week: t -> t
(**{2 Hours}

Note: no guards preventing encoding wrong hours.
*)

type s = { h : int; m : int; }
val incr_hour : ?midnight:bool -> s -> s
val now : unit -> s
val now_today : ?m:int -> unit -> s * t
val format_s : ?sep:string -> s -> string
val to_s : string -> s
type period = s * s
val to_p : string -> period

(**{2 Operations on dates and hours}*)
val add_min : s -> int -> s
val minutes : s -> int
val diff_min : s -> s -> int
val diff_days : t -> t -> int
val is_between : d:t -> t -> t -> bool

