(** driverHelpers.ml contains the drivers helper functions *) 
open Records
open Finder
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix

(** This function convert a list of strings into a string with elements
    seperated by commas. Used to print the list of items the user is allowed to
    input at each request for user input *) 
let rec list_to_string lst =
  match lst with
  | [] -> "\b\b" ^ "): "
  | h::t -> h ^ ", " ^ list_to_string t

(** This function fetches the json string according to the input HTTP address *) 
let rec retrieve_json uri =
  Lwt_main.run (body_of_uri uri)

(** This function is a helper to retrieve_json, which retrieves the body
    of the website requested *) 
and body_of_uri uri =
  Client.get (Uri.of_string uri) >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string

(** This function converts a string to a yojson object *) 
let make_yojson bod = 
  Yojson.Basic.from_string bod

(** This function formats a list of descriptions so they can be displayed
    next to the list of options in the welcome_screen function *) 
let rec get_options lst descriptions = 
  match lst with
  | [] -> ""
  | h :: t -> begin
      match descriptions with
      | [] -> ""
      | hd :: td -> begin
          "> " ^ h ^ " - " ^ hd ^ "\n" ^ get_options t td
        end
    end

(** This function displays a list of options and their descriptions 
    to help with asking the user for input *) 
let rec welcome_screen lst descriptions = 
  let intro = "Please choose from one of these options:\n" in
  let options = get_options lst descriptions in
  let prompt = ">> " in
  intro ^ options ^ prompt

(** This function ensures that the type of information the user requests is one
    of the options this program provides. If the program does not support the 
    type of information that the user requests, it continuously asks the user 
    for a correct request *) 
let rec ensure_info () first_time = 
  if first_time then begin
    let () = 
      print_string ("MAIN MENU\n" ^ welcome_screen info_lst info_des_lst) in
    let info = read_line () in begin
      if List.mem info info_lst then info
      else begin 
        let () = print_string 
            ("\n{" ^ info ^"} is not a valid option, please try again\n") in
        ensure_info () false
      end
    end
  end
  else begin
    let () = print_string (">> ") in
    let info = read_line () in begin
      if List.mem info info_lst then info
      else begin
        let () = print_string 
            ("\n{" ^ info ^"} is not a valid option, please try again\n") in 
        ensure_info () false
      end
    end
  end

(** This function returns a string of the stock price in the more visually 
    pleasing style. ex) "110.4" -> "110.40" *)
let pp_price price =
  let dot_index =  String.index_opt price '.' in begin
    match dot_index with
    | Some ind -> let len = String.length price in begin
        match len - ind with
        | 2 -> price ^ "0"
        | 1 -> price ^ "00"
        | _ -> price
      end
    | None -> price
  end

(** This function pretty-print each element of lst. *)
let pp_list pp_elt lst =
  let pp_elts lst = begin
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') -> begin
          if n = 100 then acc ^ "..."  (* stop printing long list *)
          else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
        end
    in loop 0 "" lst
  end
  in "[" ^ pp_elts lst ^ "]"

(** This function returns a string of the full company candles, in which to be 
    printed in the driver. *) 
let rec string_of_candles (cnd : company_candles) = 
  match cnd with
  | {
    opening_price_lst = [];
    closing_price_lst = [];
    high_price_lst = [];
    low_price_lst = [];
    volume_lst = [];
    date_lst = [];
  } -> ""
  | {
    opening_price_lst = h_o :: t_o;
    closing_price_lst = h_c :: t_c;
    high_price_lst = h_h :: t_h;
    low_price_lst = h_l :: t_l;
    volume_lst = h_v :: t_v;
    date_lst = h_d :: t_d;
  } -> begin 
      let data_line = "Date: " ^ string_of_date_no_time h_d ^ 
                      ", Opening Price = " ^ 
                      (h_o |> string_of_float |> pp_price) ^ 
                      ", Closing Price = " ^ 
                      (h_c |> string_of_float |> pp_price) ^ 
                      ", Highest Price = " ^ 
                      (h_h |> string_of_float |> pp_price) ^ 
                      ", Lowest Price = " ^ 
                      (h_l |> string_of_float |> pp_price) ^ 
                      ", Trade Volume = " ^ string_of_int h_v 
      in begin
        let new_cnd = {
          opening_price_lst = t_o;
          closing_price_lst = t_c;
          high_price_lst = t_h;
          low_price_lst = t_l;
          volume_lst = t_v;
          date_lst = t_d;
        } in "\n" ^ data_line ^ string_of_candles new_cnd
      end
    end
  | _ -> ""

(** This function prints the possible details the user can get about each data
    type depending on the type of record requested. *) 
let print_details info = 
  if info = "profile" then 
    let () = print_string 
        ("\n" ^ welcome_screen profile_lst profile_des_lst) in ()
  else begin 
    if info = "rec" then
      let () =  print_string 
          ("\n" ^ welcome_screen recs_lst recs_des_lst) in ()
    else begin
      if info = "quote" then
        let () = print_string 
            ("\n" ^ welcome_screen quote_lst quote_des_lst) in ()
      else begin
        if info = "candles" then
          let () = print_string 
              ("\n" ^ welcome_screen candles_lst candles_des_lst) in ()
        else begin
          if info = "targets" then
            let () = print_string 
                ("\n" ^ welcome_screen target_lst target_des_lst) in ()
        end
      end
    end
  end

(** This function ensures that the type of detail the user requests is one
    of the options this program provides. If the program does not support the
    type of detail that the user requests, it continuously asks the user for a
    correct detail *) 
let rec ensure_detail info comp =
  print_details info;
  let det = read_line () in
  print_string ("\n" ^ det ^ ": ");

  if info = "profile" && List.mem det profile_lst then
    let () = print_string (company_details (parse_company_profile comp) det); 
      print_newline() in ()
  else begin
    if info = "rec" && List.mem det recs_lst then
      let () = print_float (company_recos (parse_rec_head comp) det); 
        print_newline() in ()
    else begin
      if info = "quote" && List.mem det quote_lst then
        let () = print_string (company_qt (parse_company_quote comp) det); 
          print_newline() in ()
      else begin
        if info = "candles" && List.mem det candles_lst then
          let candles = parse_company_candles comp in
          let print_str  = ref "" in begin
            match det with 
            | "full list" -> print_str := (string_of_candles candles);
              let () = print_string (!print_str); print_newline() in ()
            | "opening price list" -> print_str := 
                (pp_list string_of_float candles.opening_price_lst);
              let () = print_string (!print_str); print_newline() in ()
            | "closing price list" -> print_str := 
                (pp_list string_of_float candles.closing_price_lst);
              let () = print_string (!print_str); print_newline() in ()
            | "high price list" -> print_str := 
                (pp_list string_of_float candles.high_price_lst);
              let () = print_string (!print_str); print_newline() in ()
            | "low price list" -> print_str := 
                (pp_list string_of_float candles.low_price_lst);
              let () = print_string (!print_str); print_newline() in ()
            | "volume list" -> print_str := 
                (pp_list string_of_int candles.volume_lst);
              let () = print_string (!print_str); print_newline() in ()
            | _ -> print_str := "";
              let () = print_string (!print_str); print_newline() in ()
          end
        else begin
          if info = "targets" && List.mem det target_lst then
            let () = print_float (company_targs(parse_company_target comp) det); 
              print_newline() in ()
          else begin
            let () = print_string 
                ("\n{" ^ det ^"} is not a valid option, please try again\n") in 
            ensure_detail info comp
          end
        end
      end
    end
  end

(** This variable prompts the user to input a ticker symbol
    and also gives an example  *) 
let ticker_details = 
  let intro = "Please enter the Ticker Symbol of the company you are \
               interested in upper case:\n" in
  let example = "* example - Apple's Ticker Symbol is AAPL\n" in
  let prompt = ">> " in
  intro ^ example ^ prompt

(** This function ensures that the company the user requests is a valid company
    in the market and forms the json from the API return.
    If the program does not support the company that the user
    requests, it continuously asks the user for a correct company.
    However, this pogram should be able to support all companies provided by
    the API which has access to every company's information, and so should
    therefore ask the user again only if a non existant company code was
    entered *) 
let rec ensure_comp info = 
  let () = print_string ("\n" ^ ticker_details) in
  let company = read_line () in
  let current_time = Unix.time() |> int_of_float |> string_of_int in
  let past_30_days = (Unix.time() -. 2592000.) |> int_of_float |> string_of_int 
  in begin

    if info = "profile" &&
       (retrieve_json ("https://finnhub.io/api/v1/stock/profile2?symbol="
                       ^ company ^ "&token=bu6l53n48v6pfj0oklv0")) <> "{}"
    then 
      (make_yojson (retrieve_json 
                      ("https://finnhub.io/api/v1/stock/profile2?symbol="
                       ^ company ^ "&token=bu6l53n48v6pfj0oklv0")))
    else begin
      if info = "rec" && 
         (retrieve_json("https://finnhub.io/api/v1/stock/recommendation?symbol="
                        ^ company ^ "&token=bu6l53n48v6pfj0oklv0")) <> "[]"
      then 
        (make_yojson (retrieve_json("https://finnhub.io/api/v1/stock/\
                                     recommendation?symbol="
                                    ^ company ^ "&token=bu6l53n48v6pfj0oklv0")))
      else begin
        if
          info = "quote" && (retrieve_json 
                               ("https://finnhub.io/api/v1/quote?symbol=" ^ 
                                company ^ "&token=bu6l53n48v6pfj0oklv0"))<> "[]"
        then
          (make_yojson (retrieve_json 
                          ("https://finnhub.io/api/v1/quote?symbol="
                           ^ company ^ "&token=bu6l53n48v6pfj0oklv0")))
        else begin
          if
            info = "candles" && (retrieve_json 
                                   ("https://finnhub.io/api/v1/stock/candle?\
                                     symbol=" ^ company ^ "&resolution=D&from="
                                    ^ past_30_days ^ "to=" ^ current_time ^ 
                                    "&token=bu6l53n48v6pfj0oklv0")) <> "[]"
          then
            (make_yojson (retrieve_json 
                            ("https://finnhub.io/api/v1/stock/candle?\
                              symbol=" ^ company ^ "&resolution=D&from="
                             ^ past_30_days ^ "&to=" ^ current_time ^ 
                             "&token=bu6l53n48v6pfj0oklv0")))
          else begin
            if
              info = "targets" && 
              (retrieve_json ("https://finnhub.io/api/v1/stock/price-target?sym\
                               bol=" ^ company ^ "&token=bu6l53n48v6pfj0oklv0")) 
              <> "[]"
            then
              (make_yojson(retrieve_json
                             ("https://finnhub.io/api/v1/stock/price-target?sym\
                               bol=" ^ company ^"&token=bu6l53n48v6pfj0oklv0")))
            else begin
              if
                info = "company news" && 
                (retrieve_json 
                   ("https://finnhub.io/api/v1/company-news?symbol="
                    ^ company ^ 
                    "&from=2020-10-30&to=2021-08-01&token=bu6l53n48v6p\
                     fj0oklv0"))
                <> "[]"
              then
                make_yojson 
                  (retrieve_json 
                     ("https://finnhub.io/api/v1/company-news?symbol=" 
                      ^ company ^ 
                      "&from=2020-10-30&to=2021-08-01&token=\
                       bu6l53n48v6pfj0oklv0"))
              else begin
                if
                  info = "peers" && 
                  (retrieve_json 
                     ("https://finnhub.io/api/v1/stock/peers?symbol=" 
                      ^ company ^ 
                      "&token=bu6l53n48v6pfj0oklv0"))
                  <> "[]"
                then
                  make_yojson 
                    (retrieve_json 
                       ("https://finnhub.io/api/v1/stock/peers?symbol=" 
                        ^ company ^ 
                        "&token=bu6l53n48v6pfj0oklv0"))
                else begin
                  let () =print_string
                      ("\n{"^company^"} is not a valid Ticker \
                                      Symbol, please try again\n")
                  in ensure_comp info
                end
              end
            end
          end
        end
      end
    end
  end

(** This function requests the amount to exchange, the base currency, and
    the to currency to request from an API how much the amount of the base
    currency is in the to currency *) 
let rec currency_ex () =
  try begin
    let () = print_string 
        "\nPlease enter amount to exchange:\n* amount can be a whole or \
         decimal number\n>> " in
    let starter = read_float() in
    let () = print_string 
        "\nPlease enter base currency code:\n* example - the United States \
         Dollar's currency code is USD\n>> " in
    let base = read_line() in
    let () = print_string 
        "\nPlease enter desired currency code:\n* example - the United States \
         Dollar's currency code is USD\n>> " in
    let toc = read_line() in
    let the_file = currency_string base toc in
    print_string (base ^ " " ^ (string_of_float starter) ^ " -> " ^ toc ^ " ");

    begin
      if the_file = "{}" 
      then let () = print_string "\nPlease enter a valid input\n" 
        in currency_ex()
      else begin
        let exchanger = 
          parse_exchange_currency 
            (make_yojson(currency_string base toc)) base toc 
        in let () = print_float(starter *. exchanger.amt) 
        in print_newline()
      end
    end
  end
  with
  | _ -> let () = print_string "Please enter a valid input\n" in currency_ex ()

(** This function is a helper to currency_ex, and creates our parsed json file
    that contains the currency conversion *) 
and currency_string base toc =
  retrieve_json ("https://free.currconv.com/api/v7/convert?q=" ^ base ^ "_"
                 ^ toc ^ "&compact=ultra&apiKey=87b6f956795f267ad993")

(** This function prints the most recent 3 news articles from the API.
    It formats it by printing each one to the console with a empty line between
    each one for readability *) 
let rec make_news info comp =
  print_newline(); 
  print_string ("RECENT TOP 3 NEWS ARTICLES");
  print_newline();
  print_string ("============================="); 
  print_newline();
  (* Loop through all elements of each news record and print them *) 
  for i = 0 to 2 do begin
    print_string ("Article #"); 
    print_int (i + 1); print_newline();
    print_string ("Publisher: ");
    print_string (company_news (List.nth (parse_news_head comp) i) "source");
    print_newline(); 
    print_string ("Headline: ");
    print_string (company_news (List.nth (parse_news_head comp) i) "headline");
    print_newline(); 
    print_string ("Summary: ");
    print_string (company_news (List.nth (parse_news_head comp) i) "summary");
    print_newline(); 
    print_string ("Article Link: ");
    print_string (company_news (List.nth (parse_news_head comp) i) "url");
    print_newline();
    print_newline();
  end
  done; () (* return unit when done *) 

(** This function prints out the overall top news article for the user *) 
let rec make_general articles =
  print_newline(); 
  print_string ("TOP NEWS");
  print_newline();
  print_string ("============="); 
  print_newline();
  (* Loop through all elements of each news record and print them *) 
  for i = 0 to 2 do begin
    print_string ("Article #"); 
    print_int (i + 1); print_newline();
    print_string ("Publisher: ");
    print_string (general_news (List.nth (parse_general_head articles) i) 
                    "publisher");
    print_newline(); 
    print_string ("Headline: ");
    print_string (general_news (List.nth (parse_general_head articles) i) 
                    "headline");
    print_newline(); 
    print_string ("Summary: ");
    print_string (general_news (List.nth (parse_general_head articles) i) 
                    "summary");
    print_newline(); 
    print_string ("Article Link: ");
    print_string (general_news (List.nth (parse_general_head articles) i) 
                    "url");
    print_newline();
    print_newline();
  end
  done; () (* return unit when done *) 

(** This function prints some examples of tickers *) 
let rec tickers tickers_list =
  match tickers_list with
  | [] -> print_newline(); 
  | h :: t -> begin
      print_newline();
      print_string (h);
      tickers t
    end

(** this function provides the help options to the user *) 
let rec help () =
  print_string ("\n" ^ welcome_screen help_lst help_des_lst);
  let info = read_line () in
  if info = "tickers" then tickers ticker_lst 
  else begin
    if info = "tutorial" then let () = print_string 
                                  ("\nOur program allows you to retrieve \
                                    desired financial data.\nPlease choose \
                                    one of the provided options and follow \
                                    the instructions to retrieve your data\
                                    . \nAlso, please keep in mind that your \
                                    inputs are case-sensitive.\n") in ()
    else if info = "glossary" then tickers glossary_lst
    else let () = print_string
             ("\n{"^info^"} is not a valid option, please try again\n")
      in help ()
  end

(**  Print the related peers *) 
let related_peers comp =
  let thelist = comp |> to_list in 
  let ocamllist = (parse_peer_list thelist) in
  print_string ("\nPEER COMPANY TICKERS: ");
  for i = 0 to ((List.length ocamllist) - 1) do begin
    print_string ((List.nth ocamllist i) ^ " "); 
  end
  done; 
  print_newline(); () (* return unit when done *) 

(**  The program Driver, which requests from the user what information they
     wish to see and prints them, all through the console print and get 
     commands *) 
let rec driver (info:string) company (first:bool) =
  if first = true then begin
    print_newline();
    let info = ensure_info () true in
    (* The QUIT, currency, and news cases are handles differently from the rest
       of the information types, since they need to request different data from
       the user, so therefore must be factored out *) 
    if info = "QUIT" then
      print_string "\nQuitting, goodbye\n\n"
    else begin
      if info = "currency" then
        let () = currency_ex () in check_currency ()
      else begin
        if info = "company news" then begin
          let comp = ensure_comp info in
          make_news info comp;
          check_news ()

        end
        else begin
          if info = "general news" then begin
            let articles = (make_yojson (retrieve_json ("\
            https://finnhub.io/api/v1/news?category=general&token=bu6l53n48v6p\
            fj0oklv0")))
            in make_general articles;
            check_general ()
          end
          else begin
            if info = "help" then begin 
              help ();
              check_help ()
            end
            else begin
              if info = "peers" then begin
                let comp = ensure_comp info
                in related_peers comp;
                check_peers ()
              end
              else begin
                let comp = ensure_comp info in
                ensure_detail info comp;
                check_again info comp
              end
            end
          end
        end
      end
    end
  end
  else begin
    print_newline();
    (* The QUIT, currency, and news cases are handles differently from the rest
       of the information types, since they need to request different data from
       the user, so therefore must be factored out *) 
    if info = "QUIT" then
      print_string "\nQuitting, goodbye\n\n"
    else begin
      if info = "currency" then
        let () = currency_ex () in check_currency ()
      else begin
        if info = "company news" then begin
          let comp = ensure_comp info in
          make_news info comp;
          check_news ()
        end
        else begin
          if info = "general news" then begin
            let articles = (make_yojson (retrieve_json ("\
            https://finnhub.io/api/v1/news?category=general&token=bu6l53n48v6p\
            fj0oklv0")))
            in make_general articles;
            check_general ()
          end
          else begin
            if info = "help" then begin 
              help ();
              check_help ()
            end
            else begin
              if info = "peers" then begin
                let comp = ensure_comp info
                in related_peers comp;
                check_peers ()
              end
              else begin
                let () = ensure_detail info company in check_again info company
              end
            end
          end
        end
      end
    end
  end

(** This function checks to see what the user wants to next after
    the currency is provided to the user *) 
and check_currency () =
  let options = [
    "currency";
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "convert another currency";
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "currency" 
  then driver "currency" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = "QUIT" 
          then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
          else begin
            let () = print_string 
                ("{"^ info ^"} is not a valid option, please try again") in
            check_currency ()
          end
        end
    end

(** This function checks to see what the user wants to next after
    the news is provided to the user *) 
and check_news () =
  let options = [
    "company news";
    "general news";
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "get news of another company";
    "latest top news"; 
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "company news" 
  then driver "company news" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = "general news" 
          then driver "general news"(make_yojson "{ \"temp\" : \"json\" }")false
          else begin
            if info = "QUIT" 
            then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
            else begin 
              let () = print_string 
                  ("{"^ info ^"} is not a valid option, please try again") in
              check_news ()
            end
          end
        end
    end

(** This function checks to see what the user wants to request more related
    companied *) 
and check_peers () =
  let options = [
    "peers";
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "get another companys related tickers";
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "peers" 
  then driver "peers" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = "QUIT" 
          then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
          else begin 
            let () = print_string 
                ("\n{"^ info ^"} is not a valid option, please try again") in
            check_peers ()
          end
        end
    end

(** This function checks to see what the user wants to next after
    the news is provided to the user *) 
and check_general () =
  let options = [
    "company news";
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "get news of a specific company";
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "company news" 
  then driver "company news" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = "QUIT" 
          then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
          else begin 
            let () = print_string 
                ("\n{"^ info ^"} is not a valid option, please try again") in
            check_general ()
          end
        end
    end

(** This function checks to see what the user wants to next after
    the help menu is provided to the user *) 
and check_help () =
  let options = [
    "help";
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "get other help";
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "help" 
  then driver "help" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = "QUIT" 
          then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
          else begin 
            let () = print_string 
                ("\n{"^ info ^"} is not a valid option, please try again") in
            check_help ()
          end
        end
    end

(** This function checks to see what the user wants to next after
    one of profile, rec, quote, targets is provided to the user *) 
and check_again original_info comp =
  let options = [
    original_info;
    "main";
    "QUIT"
  ] in
  let descriptions = [
    "get more " ^ original_info ^ " info of same company";
    "return to main menu";
    "quit program"
  ] in
  let () = print_string ("\n" ^ welcome_screen options descriptions) in
  let info = read_line () in

  if info = "QUIT" 
  then driver "QUIT" (make_yojson "{ \"temp\" : \"json\" }") false
  else 
    begin
      if info = "main" 
      then driver "main" (make_yojson "{ \"temp\" : \"json\" }") true
      else 
        begin 
          if info = original_info 
          then driver original_info comp false
          else begin 
            let () = print_string 
                ("\n{"^ info ^"} is not a valid option, please try again") in
            check_again original_info comp
          end
        end
    end