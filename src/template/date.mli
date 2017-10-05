(**{2 Dates}*)
type t
(** Abstract type for dates.*)

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
(** Month n-th *)

val bissex : int -> bool
(** Tells whether a year is a leap year. Takes cares of the current
calendar (Julian, then Gregorian since 1582.) *)
		      
val month_length : int -> int -> int
(** [month_length y m] gives the number of days in the [m]th month of
year [y]. *)
				   
val make : month * int -> t
(** Creates the date, raises [Invalid_argument] if the given data are
incorrect. *)
		    
val format : t -> string
(** dd/mm/yyyy*)
		    
val format_t : ?sep:string -> t -> string
(** Same as before, replacing the '/' if nessary by any other separator.*)
				     
val format_ymd : t -> string
(**yyyymmdd*)

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
val weekday_num : t -> int
(** Number of the weekday. Sunday is 0, Monday is 1, and so on. *)
			 
val weekend : t -> bool
(**[true] iff the date is a Saturday or a Sunday.*)

val next : ?d:day -> t -> t
(** The next [d], starting from the given date. If this is a [d],
fetches next anyway. *)
			    
val prev : ?d:day -> t -> t
(** The previous [d], starting from the given date. If this is a [d],
fetches previous anyway. *)
			    
val next_m : ?d:day -> t -> t
(** The first [d] of the next month, starting from the given
date.*)

val prev_m : ?d:day -> t -> t
(** The last [d] of the previous month, starting from the given
date.*)
		      
val next_week: t -> t
val prev_week: t -> t
(**Same day of next/previous week.*)

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

