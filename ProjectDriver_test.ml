(**
   The OUnit test file for our final project.

   Since our program relies on API's, the data of the API is always changing.
   This makes it impossible to test if our functions are returning the correct
   data from the JSON string provided by the website.

   Therefore, our approach to testing our program was to save example JSON
   results from calling the API's in our project folder. This way, we
   could check our functions against these small moments in time that our
   financial API's produced at the time, ensuring that we are parsing and
   searching the JSON's correctly.

   What we could not test, however, was if our program was correctly accessing
   the API's we wanted to use. Therefore, we had to test this manually by
   running the program, requesting the financial details we wanted, and
   comparing what was printed to the console with the actual JSON the website
   returned. To make this comparison, we also had the link to the JSON opened in
   our browsers to ensure the data was echoed in our program.

   Since the program mainly function by printing resuts to the console, its
   difficult to test the driver. Therefore, we were only able to test the
   functions in modules records and finder. All other functionality of the
   program, such as the console prints mentioned before, were tested manually by
   interacting with our program driver.

   Since we wrote the program and understand the functionality, all of our tests
   in modules records and finder were glass box testing since we understood how
   each function worked, while tests for our driver were done using black box
   testing (checking to see if the desired result was returned without needing
   to understand the implementation).

   Based on both the automated and manual testing procedures we used, we believe
   our testing suite demonstrates the correctness of the system. The automated
   portion of our testing suite ensures that our functions work correctly, and
   the manual portion of our testing suite ensures that the data is correctly
   being read from the API's and printed to the user.
*) 
open OUnit2
open ProjectDriver
open DriverHelpers
open Records
open Finder
open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix

(** Test for the details of a company profile *) 
let details_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (company_details file detail))

(** Test for the recommendations of a company *) 
let recommendations_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (company_recos file detail))

(** Test for the quote of a company *) 
let quote_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (company_qt file detail))

(** Test for the targets of a company *) 
let target_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (company_targs file detail))

(** Test for the recent news of a company *) 
let news_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (company_news file detail))

(** Test for time *)
let time_test 
    (name)
    (time) 
    (expected_output) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output time ~printer: string_of_date)

(** Test for company andles except for date *)
let candles_test
    (name)
    (time) 
    (expected_output) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output time ~printer: (pp_list string_of_float))

(** Test for company andles except for date *)
let candles_test_volume
    (name)
    (time) 
    (expected_output) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output time ~printer: (pp_list string_of_int))

(** Test for company andles except for date *)
let candles_test_date
    (name)
    (time_lst) 
    (expected_output) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pp_list string_of_date_no_time time_lst))

(** Test for the recent top news*) 
let general_test 
    (name)
    (file)
    (detail)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (general_news file detail))

(** Test for the currency convertor *) 
let curr_test 
    (name)
    (amt)
    (rate)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (amt *. rate))

(** Test for the peer companys *)
let peer_test 
    (name)
    (un)
    (expected_output) = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse_peer_list un))

(** We will use local json files to test our program, since it is impossible
    to test returned values from a live API whose results are always changing,
    with the exception of a company profile since those details are always
    constant. We will test two different companies to ensure the program
    is able to distinguish between two distinct companies/json files *) 
let apple_details_test = parse_company_profile 
    (Yojson.Basic.from_file "AppleProfileTest.json")
let apple_recs_test = parse_rec_head 
    (Yojson.Basic.from_file "AppleRecsTest.json")
let apple_quote_test = parse_company_quote 
    (Yojson.Basic.from_file "AppleQuoteTest.json")
let apple_target_test = parse_company_target 
    (Yojson.Basic.from_file "AppleTargetTest.json")
let apple_news_test = parse_news_head 
    (Yojson.Basic.from_file "AppleNewsTest.json")
let apple_candles_test = parse_company_candles 
    (Yojson.Basic.from_file "AppleCandleTest.json")

let microsoft_details_test = parse_company_profile 
    (Yojson.Basic.from_file "MicrosoftProfileTest.json")
let microsoft_recs_test = parse_rec_head 
    (Yojson.Basic.from_file "MicrosoftRecsTest.json")
let microsoft_quote_test = parse_company_quote 
    (Yojson.Basic.from_file "MicrosoftQuoteTest.json")
let microsoft_target_test = parse_company_target 
    (Yojson.Basic.from_file "MicrosoftTargetTest.json")
let microsoft_news_test = parse_news_head 
    (Yojson.Basic.from_file "MicrosoftNewsTest.json")

let general_news_test = parse_general_head 
    (Yojson.Basic.from_file "GeneralNewsTest.json")
let currency_test = parse_exchange_currency 
    (Yojson.Basic.from_file "USDtoEUR.json") "USD" "EUR" 

let peer_list_test = (Yojson.Basic.from_file "PeerTest.json" |> to_list)

(** Creates a random date *)
let random_date = {
  year = 2020;
  month = 12;
  day = 19;
  hour = 22;
  min = 32;
  sec = 13
}

(** Creates another random date *)
let random_date_est = {
  year = 2020;
  month = 12;
  day = 19;
  hour = 17;
  min = 32;
  sec = 13
}

(** Creates a random leap year *)
let random_date_leap_summertime = {
  year = 2004;
  month = 7;
  day = 6;
  hour = 21;
  min = 11;
  sec = 42
}

(** Creates a random nonleap year *)
let random_date_nonleap_nonsummertime = {
  year = 2000;
  month = 1;
  day = 15;
  hour = 15;
  min = 5;
  sec = 1;
}

(** Test the company details of Apple and Microsoft *) 
let profile_tests = [
  details_test "testing apple name" apple_details_test "name" "Apple Inc";
  details_test "testing apple industry" apple_details_test "industry" 
    "Technology";
  details_test "testing apple country" apple_details_test "country" "US";
  details_test "testing apple website" apple_details_test "website" 
    "https://www.apple.com/";
  details_test "testing apple none" apple_details_test "empty" "";
  details_test "testing null detail" apple_details_test "empty" "";

  details_test "testing microsoft name" microsoft_details_test 
    "name" "Microsoft Corp";
  details_test "testing microsoft industry" microsoft_details_test "industry" 
    "Technology";
  details_test "testing microsoft country" microsoft_details_test 
    "country" "US";
  details_test "testing microsoft website" microsoft_details_test "website" 
    "https://www.microsoft.com/en-us";
  details_test "testing microsoft none" microsoft_details_test "empty" "";
]

(** Test the recommendations of Apple and Microsoft *) 
let recs_tests = [
  recommendations_test "testing apple buy" apple_recs_test "buy" 24.;
  recommendations_test "testing apple hold" apple_recs_test "hold" 7.;
  recommendations_test "testing apple sell" apple_recs_test "sell" 0.;
  recommendations_test "testing apple strong buy" apple_recs_test 
    "strong buy" 13.;
  recommendations_test "testing apple strong sell" apple_recs_test 
    "strong sell" 0.;
  recommendations_test "testing apple none" apple_recs_test 
    "empty" 0.;

  recommendations_test "testing microsoft buy" microsoft_recs_test "buy" 16.;
  recommendations_test "testing microsoft hold" microsoft_recs_test "hold" 7.;
  recommendations_test "testing microsoft sell" microsoft_recs_test "sell" 0.;
  recommendations_test "testing microsoft strong buy" microsoft_recs_test 
    "strong buy" 16.;
  recommendations_test "testing microsoft strong sell" microsoft_recs_test 
    "strong sell" 1.;
  recommendations_test "testing microsoft none" microsoft_recs_test 
    "empty" 0.;
]

(** Test the quotes of Apple and Microsoft *) 
let quotes_tests = [
  quote_test "testing apple current price" apple_quote_test 
    "current price" "119.21";
  quote_test "testing apple opening price" apple_quote_test 
    "opening price" "119.62";
  quote_test "testing apple high price" apple_quote_test "high price" "120.53";
  quote_test "testing apple low price" apple_quote_test "low price" "118.57";
  quote_test "testing apple previous close price" apple_quote_test 
    "previous close price" "119.49";
  quote_test "testing quote date" apple_quote_test 
    "quote time" "2020/11/12 22:09:29";
  quote_test "testing none" apple_quote_test "empty" "";

  quote_test "testing microsoft current price" microsoft_quote_test 
    "current price" "218.59";
  quote_test "testing microsoft opening price" microsoft_quote_test 
    "opening price" "218.59";
  quote_test "testing microsoft high price" microsoft_quote_test 
    "high price" "219.69";
  quote_test "testing microsoft low price" microsoft_quote_test 
    "low price" "216.02";
  quote_test "testing microsoft previous close price" microsoft_quote_test 
    "previous close price" "219.42";
  quote_test "testing quote date" microsoft_quote_test 
    "quote time" "2020/12/19 17:04:03";
  quote_test "testing none" microsoft_quote_test "empty" "";
]

(** Test the targets of Apple and Microsoft *) 
let targets_tests = [
  target_test "testing apple target high" apple_target_test "target high" 150.;
  target_test "testing apple target low" apple_target_test "target low" 74.1;
  target_test "testing apple target mean" apple_target_test 
    "target mean" 123.11;
  target_test "testing apple target median" apple_target_test 
    "target median" 133.;
  target_test "testing apple none" apple_target_test "empty" 0.;

  target_test "testing microsoft target high" microsoft_target_test 
    "target high" 260.;
  target_test "testing microsoft target low" microsoft_target_test 
    "target low" 169.49;
  target_test "testing microsoft target mean" microsoft_target_test 
    "target mean" 239.37;
  target_test "testing microsoft target median" microsoft_target_test 
    "target median" 245.;
  target_test "testing microsoft none" microsoft_target_test "empty" 0.;
]

(** Test converting Unix timestamp to date *) 
let date_tests = [
  time_test "testing for random date" random_date 
    (timestamp_to_date 1608417133.);
  time_test "testing for random date eastern time" random_date_est 
    (1608417133. |> gmt_to_et |> timestamp_to_date);
  time_test "testing for random date leap year summertime" 
    random_date_leap_summertime (1089162702. |> gmt_to_et |> timestamp_to_date);
  time_test "testing for normal random date" random_date_nonleap_nonsummertime 
    (947966701. |> gmt_to_et |> timestamp_to_date);
]

(** Test each of the top 3 news articles *) 
let news_tests = [
  (* Apple news *)
  news_test "testing apple first news source" (List.nth apple_news_test 0) 
    "source" "https://hbr.org";
  news_test "testing apple first news headline" (List.nth apple_news_test 0)
    "headline" "The Rules of Co-opetition";
  news_test "testing apple first news summary" (List.nth apple_news_test 0) 
    "summary" "Rivals are working together more than ever before. Here’s how \
               to think through the risks and rewards.";
  news_test "testing apple first news url" (List.nth apple_news_test 0) 
    "url" "https://hbr.org/2021/01/the-rules-of-co-opetition";

  news_test "testing apple second news source" (List.nth apple_news_test 1) 
    "source" "https://www.bnnbloomberg.ca";
  news_test "testing apple second news headline" (List.nth apple_news_test 1)
    "headline" "Nasdaq caps a US$7-trillion decade with its best rally in 10 \
                years - BNN Bloomberg";
  news_test "testing apple second news summary" (List.nth apple_news_test 1) 
    "summary" "In a decade of extreme wealth creation in markets, few assets \
               did more to enrich investors than stocks in the Nasdaq 100 \
               Index. Their combined value jumped by more than $7 trillion, \
               ending with the best year since the bull run began.";
  news_test "testing apple second news url" (List.nth apple_news_test 1) 
    "url" "https://www.bnnbloomberg.ca/nasdaq-caps-a-us-7-trillion-decade-with-\
           its-best-rally-in-10-years-1.1368070";

  news_test "testing apple third news source" (List.nth apple_news_test 2) 
    "source" "https://www.bnnbloomberg.ca";
  news_test "testing apple third news headline" (List.nth apple_news_test 2)
    "headline" "Bryden Teich's Top Picks: Dec. 31, 2019 - BNN Bloomberg";
  news_test "testing apple third news summary" (List.nth apple_news_test 2)
    "summary" "Top picks from Bryden Teich, partner and portfolio manager \
               Avenue Investment Management.";
  news_test "testing apple third news url" (List.nth apple_news_test 2) "url"
    "https://www.bnnbloomberg.ca/bryden-teich-s-top-picks-dec-31-\
     2019-1.1368020";

  (* Microsoft news *)
  news_test "testing microsoft first news source" 
    (List.nth microsoft_news_test 0) "source" "https://www.bnnbloomberg.ca";
  news_test "testing microsoft first news headline" 
    (List.nth microsoft_news_test 0) "headline" 
    "Nasdaq caps a US$7-trillion decade with its best rally in 10 years - \
     BNN Bloomberg";
  news_test "testing microsoft first news summary" 
    (List.nth microsoft_news_test 0) 
    "summary" "In a decade of extreme wealth creation in markets, few assets \
               did more to enrich investors than stocks in the Nasdaq 100 \
               Index. Their combined value jumped by more than $7 trillion, \
               ending with the best year since the bull run began.";
  news_test "testing microsoft first news url" (List.nth microsoft_news_test 0) 
    "url" "https://www.bnnbloomberg.ca/nasdaq-caps-a-us-7-trillion-decade-with-\
           its-best-rally-in-10-years-1.1368070";

  news_test "testing microsoft second news source" 
    (List.nth microsoft_news_test 1) "source" "https://www.hindustantimes.com";
  news_test "testing microsoft second news headline" 
    (List.nth microsoft_news_test 1)
    "headline" "News updates from Hindustan Times at 9 pm: Terror-related \
                incidents, infiltration came down in 2019, says J-K DGP Dilbag \
                Singh and all the latest news at this hour";
  news_test "testing microsoft second news summary" 
    (List.nth microsoft_news_test 1) 
    "summary" "Here are today’s top news, analysis and opinion curated for \
               you. Know all about the latest news and other news updates from \
               Hindustan Times.";
  news_test "testing microsoft second news url" 
    (List.nth microsoft_news_test 1) 
    "url" "https://www.hindustantimes.com/india-news/news-updates-from-\
           hindustan-times-at-9-pm-terror-related-incidents-infiltration-came-\
           down-in-2019-says-j-k-dgp-dilbag-singh-and-all-the-latest-news-at-\
           this-hour/story-03cq6uihmPqKMKngI2zGhJ.html";

  news_test "testing microsoft third news source" 
    (List.nth microsoft_news_test 2) "source" "https://www.bnnbloomberg.ca";
  news_test "testing microsoft third news headline" 
    (List.nth microsoft_news_test 2) "headline" 
    "From Encana to 'bungled' legal pot: BNN Bloomberg talent on top business \
     stories in 2019 - BNN Bloomberg";
  news_test "testing microsoft third news summary" 
    (List.nth microsoft_news_test 2) "summary" 
    "BNN Bloomberg personalities make their cases for the top story \
     of the year, the most compelling business leader, the biggest \
     blunder was, and which companies you should watch in the new \
     year.";
  news_test "testing microsoft third news url" (List.nth microsoft_news_test 2) 
    "url" "https://www.bnnbloomberg.ca/from-encana-to-bungled-legal-pot-bnn-\
           bloomberg-talent-on-top-business-stories-in-2019-1.1367539";

  (* General market news *)
  general_test "testing general first news source" 
    (List.nth general_news_test 0)  "publisher" "CNBC";
  general_test "testing general first news headline" 
    (List.nth general_news_test 0)
    "headline" "$600 second stimulus checks could be coming. Here's who's \
                first in line to get them";
  general_test "testing general first news summary" 
    (List.nth general_news_test 0) 
    "summary" "If Congress passes a new Covid relief package, a second round \
               of stimulus checks could go out this month. But the timeline \
               may be faster for some.";
  general_test "testing general first news url" (List.nth general_news_test 0) 
    "url" "https://www.cnbc.com/2020/12/19/second-stimulus-check-when-will-i-\
           get-a-covid-relief-cash-payment-and-how.html";

  general_test "testing general second news source" 
    (List.nth general_news_test 1) "publisher" "CNBC";
  general_test "testing general second news headline" 
    (List.nth general_news_test 1)
    "headline" "Trump contradicts Pompeo, plays down alleged Russian role in \
                cyberattack";
  general_test "testing general second news summary" 
    (List.nth general_news_test 1) 
    "summary" "The company at the center of the attack, SolarWinds, has not \
               assigned blame to any country thus far.";
  general_test "testing general second news url" (List.nth general_news_test 1) 
    "url" "https://www.cnbc.com/2020/12/19/trump-contradicts-pompeo-plays-\
           down-alleged-russian-role-in-hack.html";

  general_test "testing general third news source" 
    (List.nth general_news_test 2) "publisher" "CNBC";
  general_test "testing general third news headline" 
    (List.nth general_news_test 2)
    "headline" "'The Mandalorian' launched Disney+, now it has the goodwill \
                to usher in a new era of Star Wars";
  general_test "testing general third news summary" 
    (List.nth general_news_test 2)
    "summary" "Under the careful direction of showrunners Jon Favreau and \
               Dave Filoni, \"The Mandalorian\" has reignited faith in the \
               Star Wars franchise.";
  general_test "testing general third news url" (List.nth general_news_test 2) 
    "url" "https://www.cnbc.com/2020/12/19/the-mandalorian-is-ushering-in-new-\
           era-of-star-wars-on-disney.html";
]

(** Test the candles of Apple *) 
let candles_tests = [
  candles_test "testing for openinng price" apple_candles_test.opening_price_lst
    [120.5; 115.57; 117.17; 119.55; 119.44; 118.93; 119.51] ;
  candles_test "testing for closing price" apple_candles_test.closing_price_lst
    [116.32; 115.97; 119.49; 119.21; 119.26; 120.3; 119.39] ;
  candles_test "testing for high price" apple_candles_test.high_price_lst
    [121.98; 117.58; 119.59; 120.52; 119.67; 120.99; 120.67];
  candles_test "testing for low price" apple_candles_test.low_price_lst
    [116.12; 114.14; 116.51; 118.58; 117.87; 118.17; 118.99];
  candles_test_volume "testing for volume" apple_candles_test.volume_lst
    [154515312; 138023392; 112294952; 103350672; 81689000; 91183016; 74270976];
  candles_test_date "testing for date" apple_candles_test.date_lst
    "[2020/11/09; 2020/11/10; 2020/11/11; 2020/11/12; 2020/11/13; 2020/11/16; \
     2020/11/17]" 
]

(** Test the conversion rate *) 
let currencys_tests = [
  curr_test "testing conversion result" 5.0 currency_test.amt 4.079135;
  curr_test "testing conversion result" 0.0 currency_test.amt 0.;
]

(** Test the peer result *) 
let peer_tests = [
  peer_test "check list" peer_list_test ["AAPL";"DELL";"HPQ";"1337.HK";"WDC";
                                         "HPE";"NTAP";"PSTG";"XRX";"NCR"]
]

(** Call each of the testing suites *) 
let suite =
  "automatic test suite for final project"  >::: List.flatten [
    profile_tests;
    recs_tests;
    quotes_tests;
    targets_tests;
    date_tests;
    news_tests;
    candles_tests;
    currencys_tests;
    peer_tests
  ]

(** Run the testing suite *) 
let _ = run_test_tt_main suite