(** records.ml contains the functions that help in parsing jsons and creating
    records out of them for the program to store data in *) 
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix

(** List of types information the program supports *) 
let info_lst = [
  "profile"; 
  "rec";
  "quote";
  "candles"; 
  "targets";
  "company news";
  "general news"; 
  "peers";
  "currency";
  "help";
  "QUIT"
]

(** List of descriptions of types information the program supports *) 
let info_des_lst = [
  "general information";
  "latest analyst recommendation trends"; 
  "real-time quote data [US stocks only]"; 
  "stock candles data of the past 30 days [US Stocks only]";
  "latest price target consensus"; 
  "latest company specific news [North American companies only]"; 
  "latest top news"; 
  "find related companys";
  "currency conversion"; 
  "get helpful information";
  "quit program"
]

(** List of fields in the profile record *) 
let profile_lst = [
  "name";
  "industry";
  "country";
  "website"
]

(** List of descriptions fields in the profile record *) 
let profile_des_lst = [
  "company name";
  "company industry";
  "country of company's headquarter";
  "company website"
]

(** List of fields in the recommendation record *) 
let recs_lst = [
  "buy";
  "hold";
  "sell";
  "strong buy";
  "strong sell"
]

(** List of descriptions fields in the recommendation record *) 
let recs_des_lst = [
  "number of recommendations for buy";
  "number of recommendations for hold";
  "number of recommendations for sell";
  "number of recommendations for strong buy";
  "number of recommendations for strong sell"
]

(** List of fields in the quote record *) 
let quote_lst = [
  "current price";
  "opening price";
  "high price";
  "low price";
  "previous close price";
  "quote time"
]

(** List of descriptions fields in the quote record *) 
let quote_des_lst =[
  "current price";
  "open price of the day";
  "high price of the day";
  "low price of the day";
  "previous close price";
  "date and time the quote was retrieved"
]

(** List of fields in the candles record *) 
let candles_lst = [
  "full list";
  "opening price list";
  "closing price list";
  "high price list";
  "low price list";
  "volume list";
]

(** List of descriptions fields in the candles record *) 
let candles_des_lst = [
  "full data of the past 30 days";
  "list of opening prices for each day";
  "list of closing prices for each day";
  "list of highest prices for each day";
  "list of lowest prices for each day";
  "list of volume of trading for each day";
]

(** List of fields in the targets record *) 
let target_lst = [
  "target high";
  "target low";
  "target mean";
  "target median"
]

(** List of descriptions fields in the target record *) 
let target_des_lst =[
  "highest analysts' target";
  "lowest analysts' target";
  "mean of all analysts' targets";
  "median of all analysts' targets";
]

(** List of fields in the help record *) 
let help_lst = [
  "tutorial";
  "tickers";
  "glossary"
]

(** List of descriptions fields in the help record *) 
let help_des_lst = [
  "how to use this program";
  "get ticker symbols of the 10 most popular companies";
  "get definitions for common terms"
]

(** List of fields in the ticker record *) 
let ticker_lst = [
  "Apple - AAPL";
  "Amazon - AMZN";
  "Microsoft - MSFT";
  "Tesla - TSLA";
  "Johnson & Johnson - JNJ";
  "Google - GOOGL";
  "Facebook - FB";
  "JPMorgan Chase - JPM";
  "Visa - V";
  "Procter & Gamble - PG"
]

(** List of fields in the glossary record *) 
let glossary_lst = [
  "Bear Market - when the stock market is in a downward trend";
  "Bull Market - when the stock market is in an upward trend";
  "Day Trading - buying and selling within the same trading day";
  "Dividend - portion of a company’s earnings paid to shareholders";
  "Dividend Yield - return on an investment as dividend";
  "Initial Public Offering (IPO) - when a private company becomes public";
  "Penny Stocks - generall stocks that are under $5";
  "Price to Earnings Ratio - current price of each share over its earnings";
  "Sector - a group of stocks that are in the same industry";
  "Short Selling - borrowing and selling shares";
]

(** This record represent the company profile, where the details the program
    supports will be stored into for each company*) 
type company_profile = {
  name : string;
  industry : string;
  country : string;
  website : string
}

(** This function parses the company profile from a json into the profile record
    that the program can pull information from *) 
let parse_company_profile json = {
  name = json |> member "name" |> to_string;
  industry = json |> member "finnhubIndustry" |> to_string;
  country = json |> member "country" |> to_string;
  website = json |> member "weburl" |> to_string;
}

(** This record represent the company recommendations, where the details the
    program supports will be stored into for each company*) 
type company_recs = {
  buy : float;
  hold : float;
  sell : float;
  strong_buy : float;
  strong_sell : float;
}

(** This function parses the company recommendations from a json into the
    recommendations record that the program can pull information from *) 
let parse_company_recs json = {
  buy = json |> member "buy" |> to_number;
  hold = json |> member "hold" |> to_number;
  sell = json |> member "sell" |> to_number;
  strong_buy = json |> member "strongBuy" |> to_number;
  strong_sell = json |> member "strongSell" |> to_number;
}

(** This function allows for the parsing of a list of company recommendations
    with multiple timestamps. Since the program only cares about the most recent
    recommendations, only the head o the list is parsed *) 
let parse_rec_head json = 
  let list_rec = Yojson.Basic.Util.to_list json in
  match list_rec with
  | [] -> begin {
      buy = 0.;
      hold = 0.;
      sell = 0.;
      strong_buy = 0.;
      strong_sell = 0.;
    }
    end
  | h :: t -> parse_company_recs h

(** This record represent the company news, where the details the
    program supports will be stored into for each company *) 
type company_news = {
  source : string;
  headline : string;
  summary : string;
  url : string;
}

(** This function parses the company news from a json into the news record that
    the program can pull information from *) 
let parse_company_news json = {
  source = json |> member "source" |> to_string;
  headline = json |> member "headline" |> to_string;
  summary = json |> member "summary" |> to_string;
  url = json |> member "url" |> to_string;
}

(** This function allows for the parsing of a list of company news with multiple
    timestamps. Since the program only cares about the most recent news, only
    the three most recent articles is parsed *) 
let parse_news_head json = 
  let list_rec = Yojson.Basic.Util.to_list json in
  match list_rec with
  | h :: h1 :: h2 :: t -> begin [parse_company_news h; parse_company_news h1;
                                 parse_company_news h2]
    end
  | _ -> begin [{
      source = "";
      headline = "";
      summary = "";
      url = "";
    }]
    end

(** This record represent the general news, where the details the
    program supports will be stored into for te current news *) 
type general_news = {
  publisher : string;
  headline : string;
  summary : string;
  url : string;
  extra : string;
}

(** This function parses the general news from a json into a news record that
    the program can pull information from *) 
let parse_general_news json = {
  publisher = json |> member "source" |> to_string;
  headline = json |> member "headline" |> to_string;
  summary = json |> member "summary" |> to_string;
  url = json |> member "url" |> to_string;
  extra = "";
}

(** This function allows for the parsing of a list of general news with multiple
    timestamps. Since the program only cares about the most recent news, only
    the three most recent articles is parsed *) 
let parse_general_head json = 
  let list_rec = Yojson.Basic.Util.to_list json in
  match list_rec with
  | h :: h1 :: h2 :: t -> begin [parse_general_news h; parse_general_news h1;
                                 parse_general_news h2]
    end
  | _ -> begin [{
      publisher = "";
      headline = "";
      summary = "";
      url = "";
      extra = "";
    }]
    end

(** This record represent date, which stores the information 
    of Unix Timestamp *)
type date = {
  year : int;
  month : int;
  day : int;
  hour : int;
  min : int;
  sec : int
}

(** Creates a date with Unix timestamp tm *) 
let timestamp_to_date tm =
  let date = Unix.gmtime tm in begin
    {
      year = date.tm_year + 1900;
      month = date.tm_mon + 1;
      day = date.tm_mday;
      hour = date.tm_hour;
      min = date.tm_min;
      sec =  date.Unix.tm_sec;
    }
  end

(** This record represents the company quote, where the details the
    program supports will be stored into for each company *) 
type company_quote = {
  current_price : float;
  opening_price : float;
  high_price : float;
  low_price : float;
  previous_close_price : float;
  quote_time : date;
}

(** Returns true if the Unix timestamp(in GMT) is during daylight savings or 
    not. Assumed at the second Sunday of March and first sunday of November
    2:00AM ET, the clock moves forward and backwards 1 hour.*)
let daylight_savings (gmt_dt : float) = 
  let dt = Unix.gmtime (gmt_dt -. 14400.) in
  if dt.tm_mon > 2 && dt.tm_mon < 10
  then true
  else if dt.tm_mon == 2
  then begin
    if dt.tm_wday = 0 
    then begin
      if dt.tm_mday > 7 && dt.tm_mday < 15 && dt.tm_hour >= 2
      then true
      else false
    end
    else begin
      let sun_of_week = dt.tm_mday - dt.tm_wday in
      if sun_of_week - 7 > 0 
      then true 
      else false
    end
  end
  else if dt.tm_mon == 10
  then begin
    if dt.tm_mday < 8 
    then begin 
      if dt.tm_wday = 0
      then begin
        if dt.tm_hour < 2 
        then true 
        else false
      end
      else begin
        let sun_of_week = dt.tm_mday - dt.tm_wday in
        if sun_of_week <= 0
        then true
        else false
      end
    end
    else false
  end
  else false

(** Converts GMT time stamp into ET time stamp *) 
let gmt_to_et tm = 
  if daylight_savings tm
  then tm -. 14400.
  else tm -. 18000.

(** This function rounds a float to its hundreth decimal.*)
let round_hundredth x = 
  let x100 = floor (x *. 100. +. 0.5) in
  x100 /. 100.


(** This function parses the company quote from a json into the quote record
    that the program can pull information from. Quote time is in ET. *)
let parse_company_quote json = {
  current_price = json |> member "c" |> to_number |> round_hundredth;
  opening_price = json |> member "o" |> to_number |> round_hundredth;
  high_price = json |> member "h" |> to_number |> round_hundredth;
  low_price = json |> member "l" |> to_number |> round_hundredth;
  previous_close_price = json |> member "pc" |> to_number |> round_hundredth;
  quote_time = json |> member "t" |> to_number |> gmt_to_et 
               |> timestamp_to_date;
}

(** This record represents the historical company stock prices.*)
type company_candles = {
  opening_price_lst : float list;
  closing_price_lst : float list;
  high_price_lst : float list;
  low_price_lst : float list;
  volume_lst : int list;
  date_lst : date list;
}

(** This function parses the company candles from a json into the candles record
    that the program can pull information from. Quote time is in ET. *)
let parse_company_candles json = {
  opening_price_lst = json |> member "o" |> to_list 
                      |> List.map to_number |> List.map round_hundredth;
  closing_price_lst = json |> member "c" |> to_list 
                      |> List.map to_number |> List.map round_hundredth;
  high_price_lst = json |> member "h" |> to_list 
                   |> List.map to_number |> List.map round_hundredth;
  low_price_lst = json |> member "l" |> to_list 
                  |> List.map to_number |> List.map round_hundredth;
  volume_lst =  json |> member "v" |> to_list |> List.map to_int;
  date_lst = json |> member "t" |> to_list 
             |> List.map to_number |> List.map timestamp_to_date;
}

(** This record represent the exchange rate for a currency conversion *) 
type exchange = {
  amt : float;
}

(** This function parses the currency exchange from a json into the exchange
    record that the program can pull information from *) 
let parse_exchange_currency json base toc= {
  amt = json |> member (base ^ "_" ^ toc) |> to_number;
}

(** This record represents the company target, where the details the
    program supports will be stored into for each company *) 
type company_target = {
  targethigh : float;
  targetlow : float;
  targetmean : float;
  targetmedian : float;
}

(** This function parses the company targets from a json into the targets
    record that the program can pull information from *) 
let parse_company_target json = {
  targethigh = json |> member "targetHigh" |> to_number;
  targetlow= json |> member "targetLow" |> to_number;
  targetmean = json |> member "targetMean" |> to_number;
  targetmedian = json |> member "targetMedian" |> to_number;
}

(** This function allows for the parsing of a list of peers *) 
let rec parse_peer_list list_rec = 
  match list_rec with
  | [] -> []
  | h :: t -> (h |> to_string) :: (parse_peer_list t)