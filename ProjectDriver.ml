(** ProjectDriver.ml contains the driver of the program*) 
open Records
open Finder
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open DriverHelpers

(** This _ allows the program to execute on make build, calling our driver
    program to begin *) 
let _ = begin
  print_newline(); 
  print_string("FINANCIAL DATA PROGRAM"); 
  print_newline();
  print_string("========================="); 
  (driver "a" (make_yojson "{ \"temp\" : \"json\" }") true)
end