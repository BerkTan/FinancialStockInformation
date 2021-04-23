(** finder.ml contains the pattern matching functions that help us return the
    specific details the user requests from records *) 
open Records
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix

(** This function returns the desired detail of the company profile that the
    user requests *) 
let company_details file detail =
  match detail with
  | "name" -> file.name
  | "industry" -> file.industry
  | "country" -> file.country
  | "website" -> file.website
  | _ -> ""

(** This function returns the desired detail of the company recommendations that
    the user requests *) 
let company_recos file detail =
  match detail with
  | "buy" -> file.buy
  | "hold" -> file.hold
  | "sell" -> file.sell
  | "strong buy" -> file.strong_buy
  | "strong sell" -> file.strong_sell
  | _ -> 0.

(** This function returns the desired detail of the company news that
    the user requests *) 
let company_news file detail =
  match detail with
  | "source" -> file.source
  | "headline" -> file.headline
  | "url" -> file.url
  | "summary" -> file.summary
  | _ -> ""

(** This function returns the desired detail of the company news that
    the user requests *) 
let general_news file detail =
  match detail with
  | "publisher" -> file.publisher
  | "headline" -> file.headline
  | "url" -> file.url
  | "summary" -> file.summary
  | _ -> ""

(** This function makes sure the string of an int is returned as a two digit 
    representation. *)
let two_digit int =
  if int < 10
  then "0" ^ string_of_int int
  else string_of_int int

(** This function converts date with time to a string representation of 
    "YEAR/MM/DD HR:MN:SC" *)
let string_of_date dt =
  string_of_int dt.year ^ "/" ^ (two_digit dt.month) ^ "/" ^ (two_digit dt.day) 
  ^ " " ^ (two_digit dt.hour) ^ ":" ^ (two_digit dt.min) ^ ":" ^ 
  (two_digit dt.sec)

(** This function converts date without time to a string representation of 
    "YEAR/MM/DD" *)
let string_of_date_no_time dt =
  string_of_int dt.year ^ "/" ^ (two_digit dt.month) ^ "/" ^ (two_digit dt.day)

(** This function returns the desired detail of the company quote that
    the user requests *) 
let company_qt file detail = 
  match detail with
  | "current price" -> string_of_float file.current_price
  | "opening price" -> string_of_float file.opening_price
  | "high price" -> string_of_float file.high_price
  | "low price" -> string_of_float file.low_price
  | "previous close price" -> string_of_float file.previous_close_price
  | "quote time" -> string_of_date file.quote_time
  | _ -> ""

(** This function returns the desired detail of the company targets that
    the user requests *) 
let company_targs file detail =
  match detail with
  | "target high" -> file.targethigh
  | "target low" -> file.targetlow
  | "target mean" -> file.targetmean
  | "target median" -> file.targetmedian
  | _ -> 0.0