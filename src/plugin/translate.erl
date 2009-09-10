% ex: set ts=2 et:
m  

-module(translate).
-author("pizza@parseerror.com").
-export(
  [
    test/0,
    loop/0,
    charswap/4,
    en_aol/1,
    aol_en/1
  ]).

-include_lib("../irc.hrl").
-import(test).
-import(irc).
-import(cgi).

-define(timeout, 5000).
-define(babelfish_url, "http://babelfish.yahoo.com/translate_txt").

test() ->
  true.

loop() ->
  receive
    { act, Pid, _Irc, Msg, Dst, Nick, ["translate", Lang, Lang | Txt]} ->
      % same language
      Pid ! {pipe, Msg, irc:resp(Dst, Nick, Txt) },
      loop();
    { act, Pid, Irc, Msg, Dst, Nick, ["translate", FromLang, ToLang | Txt]} ->
      xl(Pid, Irc, Msg, Dst, Nick, FromLang, ToLang, Txt),
      loop();
    %{ act, Pid, Irc, _, Dst, Nick, ["translate", "attach", Who, FromLang, ToLang]} ->
    % TODO: attach feature
    %  attach(Pid, Irc, Dst, Nick, Who, FromLang, ToLang),
    %  loop();
    { act, _, _, _, _, _, _ } ->
      loop();
    { help, Pid, Dst, Nick } ->
      Pid ! {q,
        [ irc:resp(Dst, Nick, Nick ++ ": " ++ Que) || Que <-
          [ "[\"translate\", From, To | Txt ] -> Translate Txt From to To",
            %"[\"translate\", \"attach\", Who, From, To] -> Automatically Translate To/From for Who",
            "Lang = [" ++ util:join(",", [ Short || {Short,_} <- langs() ]) ++ "]",
            "Example: translate en es hello",
            "NOTE: not all languages work in both directions"
          ]
        ]},
      loop()
  end.

xl(Pid, _, Msg, Dst, Nick, "en", "engrish", Txt_) ->
  Txt = util:j(Txt_),
  Result = charswap(Txt, [], $l, $r),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };
xl(Pid, _, Msg, Dst, Nick, "en", "aol", Txt_) ->
  Txt = util:j(Txt_),
  Result = en_aol(Txt),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };
xl(Pid, _, Msg, Dst, Nick, "aol", "en", Txt_) ->
  Txt = util:j(Txt_),
  Result = aol_en(Txt),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };
xl(Pid, _, Msg, Dst, Nick, "en", "eb", Txt_) ->
  Txt = util:j(Txt_),
  Result = en_eb(Txt),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) };
xl(Pid, _Irc, Msg, Dst, Nick, "chat", "en", Txt) ->
  % TODO: don't reload the file fresh every time, sheesh
  % plugins should have an "init" function or sumthn
  % where they load data into Irc's state
  Dict = load_internet_slang("data/translate/chat"),
  Result = util:j(internet_slang(Txt, [], Dict)),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result)};
xl(Pid, _Irc, Msg, Dst, Nick, "/b/", "en", Txt) ->
  % TODO: don't reload the file fresh every time, sheesh
  % plugins should have an "init" function or sumthn
  % where they load data into Irc's state
  Dict = load_internet_slang("data/translate/internet-slang"),
  Result = util:j(internet_slang(Txt, [], Dict)),
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result)};
xl(Pid, _Irc, Msg, Dst, Nick, From, To, Txt_) ->
  Txt = util:j(Txt_),
  io:format("translate From=~p To=~p Txt_=~p Txt=~p~n",
    [From, To, Txt_, Txt]),
  {Code, Content} = content(From, To, Txt),
  io:format("translate Code=~p~n",[Code]),
  case Code of
    error ->
      Pid ! {q, irc:resp(Dst, Nick,
        lists:flatten(
          io_lib:format("translate -> ~s", [Code])))};
    _ -> ok(Pid, Msg, Dst, Nick, Content)
  end.

content(From, To, Txt) ->
  Post =
    [
     { "ei",      "ISO-8859-1"      },
     { "doit",     "done"            },
     { "fr",      "bf-res"          },
     { "intl",    "1"               },
     { "tt",      "urltext"         },
     { "lp",      From ++ "_" ++ To  },  
     { "trtext",  Txt               },
     { "inp_btn",  "Translate"        }],
  io:format("Header Post=~p~n", [Post]),
  case http:request(post,
    {
      ?babelfish_url,                                        % URL
      [{ "Referer", ?babelfish_url },                       % Headers
       { "User-Agent",
          "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)" }],
      "application/x-www-form-urlencoded",                  % Encoding
        util:join("&",
          [ K ++ "=" ++ cgi:url_encode(V) || {K,V} <- Post ])
    },
    [{timeout, ?timeout}],
    [{body_format, binary}]
  ) of
    {ok, {{_Http, Code, _Msg}, _Header, Content}} -> {Code, Content};
    {error, Why} -> {error, Why}
    end.

ok(Pid, Msg, Dst, Nick, Contents) ->
  Result = result(Contents),  % FIXME: normalize result
                              % NOTE: result may be in crazy different char set
  Pid ! {pipe, Msg, irc:resp(Dst, Nick, Result) }.

result([]) -> [];
result(Contents) ->
  Doc = mochiweb_html:parse(Contents),
  Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
  F = fun(_Ctx, [String]) -> % mysterious
    proplists:get_value(String, Mapping, 0) end,
  MyFuns = [{my_fun, F, [string]}],
  Result = mochiweb_xpath:execute("//div[@id='result']/div/text()", Doc, MyFuns),
  io:format("translate Result=~p~n", [Result]),
  Res = if
    (length(Result) > 0) ->
      R = hd(Result),
      if is_binary(R) -> binary_to_list(R);
        true -> R
      end;
    true -> []
  end,
  io:format("translate Res=~p~n", [Res]),
  Res.

%attach(Pid, Irc, Dst, Nick, Who, FromLang, ToLang) ->
%  nil.

langs() ->
  [
    { "aol","Aolspeak"      },
    { "/b/","/b/speak"      },
    { "chat", "Chat/IM/Txt" },
    { "de", "German"        },
    { "eb", "Ebonics"       },
    { "el", "Greek"         },
    { "engrish","Engrish"   },
    { "es", "Spanish"       },
    { "fr", "French"        },
    { "it", "Italian"       },
    { "ja", "Japanese"      },
    { "ko", "Korean"        },
    { "nl", "Dutch"         },
    { "pt", "Portugese"     },
    { "ru", "Russian"       },
    { "zh", "Chinese-simp"  },
    { "zt", "Chinese"       }
  ].

% utility functions for silly easter eggs

charswap([],           Wr, _, _) -> Wr;
charswap([A|Rd],       Wr, A, B) -> charswap(Rd, Wr ++ [B],      A, B);
charswap([B|Rd],       Wr, A, B) -> charswap(Rd, Wr ++ [A],      A, B);
charswap("City" ++ Rd, Wr, A, B) -> charswap(Rd, Wr ++ "Shitty", A, B);
charswap("city" ++ Rd, Wr, A, B) -> charswap(Rd, Wr ++ "shitty", A, B);
charswap("sh " ++ Rd,  Wr, A, B) -> charswap(Rd, Wr ++ "s ",     A, B);
charswap([X|Rd],       Wr, A, B) -> charswap(Rd, Wr ++ [X],      A, B).

% TODO: move data out to file
en_aol(Str) -> en_aol(string:to_upper(Str), []).
en_aol([],            Wr) -> Wr ++ "!!!1";
en_aol([$A|Rd],       Wr) -> en_aol(Rd, Wr ++ [$@]);
en_aol([$B|Rd],       Wr) -> en_aol(Rd, Wr ++ [$|, $3]);
en_aol([$I|Rd],       Wr) -> en_aol(Rd, Wr ++ [$1]);
en_aol([$K|Rd],       Wr) -> en_aol(Rd, Wr ++ [$|, $<]);
en_aol([$O|Rd],       Wr) -> en_aol(Rd, Wr ++ [$0]);
en_aol([$S|Rd],       Wr) -> en_aol(Rd, Wr ++ [$$]);
en_aol("hello" ++ Rd, Wr) -> en_aol(Rd, Wr ++ "HEL0");
en_aol("the"   ++ Rd, Wr) -> en_aol(Rd, Wr ++ "D@");
en_aol("too"   ++ Rd, Wr) -> en_aol(Rd, Wr ++ "2");
en_aol("to"    ++ Rd, Wr) -> en_aol(Rd, Wr ++ "2");
en_aol("you"   ++ Rd, Wr) -> en_aol(Rd, Wr ++ "U");
en_aol("haha"  ++ Rd, Wr) -> en_aol(Rd, Wr ++ "LULZ!");
en_aol("very"  ++ Rd, Wr) -> en_aol(Rd, Wr ++ "WAI");
en_aol("way"   ++ Rd, Wr) -> en_aol(Rd, Wr ++ "WAI");
en_aol([R|Rd],        []) -> en_aol(Rd, [R]);
en_aol([R|Rd],        Wr) -> en_aol(Rd, Wr ++ [R]).

% TODO: move data out to file
aol_en(Str) ->
  Lc = string:to_lower(Str),
  aol_en(string:tokens(Lc, " ?!1"), []).
aol_en([],            Wr) -> util:j(Wr);
aol_en("da"    ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["the"]);
aol_en("omg"   ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["oh my"]);
aol_en("helo"  ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["hello"]);
aol_en("lulz"  ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["haha"]);
aol_en("noes"  ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["no"]);
aol_en("wtf"   ++ Rd, Wr) -> aol_en(Rd, Wr);
aol_en("lol"   ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["haha"]);
aol_en("ur"    ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["your"]);
aol_en("u"     ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["you"]);
aol_en("r"     ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["are"]);
aol_en("wai"   ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["way"]);
aol_en("m"     ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["am"]);
aol_en("awsum" ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["awesome"]);
aol_en("fien"  ++ Rd, Wr) -> aol_en(Rd, Wr ++ ["fine"]);
aol_en([R|Rd], []) -> aol_en(Rd, [R]);
aol_en([R|Rd], Wr) -> aol_en(Rd, Wr ++ [R]).

% "HELO HOW R U??!?! WTF IM FIEN!11!1!11 WTF"
% "OMG I M SO FCKIG AWSUM!!!!!1 CUM WURSHP MEE BCH!!!!!1" 

% FIXME: put this crap in a file
en_eb(Str) ->
  Lc = string:to_lower(Str),
  Tok = string:tokens(Lc, " ?:,"),
  en_eb(Tok, []).
en_eb([], Wr) -> util:j(Wr);
en_eb(["nearby","residential","area" | Rd], Wr) -> en_eb(Rd, Wr ++ ["da hood"]);
en_eb(["understand","this","fact"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["get it now"]);
en_eb(["where","are","you"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["where you at"]);
en_eb(["would","you","like"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["yo lets"]);
en_eb(["i","must","warn","you"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["recognize"]);
en_eb(["pay","attention"             | Rd], Wr) -> en_eb(Rd, Wr ++ ["recognize"]);
en_eb(["white","people"              | Rd], Wr) -> en_eb(Rd, Wr ++ ["crackas"]);
en_eb(["my","goodness"               | Rd], Wr) -> en_eb(Rd, Wr ++ ["snap"]);
en_eb(["watch","carefully"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["peep this"]);
en_eb(["watch","this"                | Rd], Wr) -> en_eb(Rd, Wr ++ ["peep this"]);
en_eb(["watch","me"                  | Rd], Wr) -> en_eb(Rd, Wr ++ ["peep this"]);
en_eb(["oh","my","gosh"              | Rd], Wr) -> en_eb(Rd, Wr ++ ["oh damn!"]);
en_eb(["oh","my","goddness"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["oh damn!"]);
en_eb(["associated","with"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["all up in"]);
en_eb(["associated"   | Rd], Wr) -> en_eb(Rd, Wr ++ ["all up in"]);
en_eb(["i","have"     | Rd], Wr) -> en_eb(Rd, Wr ++ ["i gots"]);
en_eb(["i","will"     | Rd], Wr) -> en_eb(Rd, Wr ++ ["gunna"]);
en_eb(["going","to"   | Rd], Wr) -> en_eb(Rd, Wr ++ ["gunna"]);
en_eb(["the"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["da"]);
en_eb(["or"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["o"]);
en_eb(["to"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["all"]);
en_eb(["sure"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["sho"]);
en_eb(["for"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["fo"]);
en_eb(["my"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["ma"]);
en_eb(["hello"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["wassup"]);
en_eb(["greetings"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["wassup"]);
en_eb(["deal"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["dilly"]);
en_eb(["man"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["muthafucka"]);
en_eb(["friend"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["nigga"]);
en_eb(["friends"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["peeps"]);
en_eb(["associates"   | Rd], Wr) -> en_eb(Rd, Wr ++ ["peeps"]);
en_eb(["gentleman"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["g"]);
en_eb(["gentlemen"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["gs"]);
en_eb(["gangster"     | Rd], Wr) -> en_eb(Rd, Wr ++ ["g"]);
en_eb(["sir"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["nigga"]);
en_eb(["brother"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["nigga"]);
en_eb(["nigger"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["nizzle"]);
en_eb(["madam"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["bitch"]);
en_eb(["whore"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["ho"]);
en_eb(["women"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["bitches"]);
en_eb(["woman"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["bitch"]);
en_eb(["girl"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["bitch"]);
en_eb(["?"            | Rd], Wr) -> en_eb(Rd, Wr ++ ["yo?"]);
en_eb(["accessories"  | Rd], Wr) -> en_eb(Rd, Wr ++ ["bling"]);
en_eb(["jewelry"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["bling"]);
en_eb(["need"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["gots ta git"]);
en_eb(["want"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["gots ta git"]);
en_eb(["money"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["Benjamins"]);
en_eb(["good"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["bomb"]);
en_eb(["marijuana"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["chronic"]);
en_eb(["weed"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["chronic"]);
en_eb(["beer"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["40z"]);
en_eb(["here"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["up in this piece"]);
en_eb(["copious"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["mad"]);
en_eb(["many"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["mad"]);
en_eb(["lots","of"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["mad"]);
en_eb(["lots"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["mad"]);
en_eb(["alcohol"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["cristal"]);
en_eb(["boyfriend"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["boo"]);
en_eb(["girlfriend"   | Rd], Wr) -> en_eb(Rd, Wr ++ ["boo"]);
en_eb(["husband"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["man"]);
en_eb(["listen","up"  | Rd], Wr) -> en_eb(Rd, Wr ++ ["check it"]);
en_eb(["listen"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["check it"]);
en_eb(["promiscuous"  | Rd], Wr) -> en_eb(Rd, Wr ++ ["ho"]);
en_eb(["have"         | Rd], Wr) -> en_eb(Rd, Wr);
en_eb(["intercourse"  | Rd], Wr) -> en_eb(Rd, Wr ++ ["fuck"]);
en_eb(["car"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["benz"]);
en_eb(["sex"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["fuck"]);
en_eb(["ex-girlfriend"| Rd], Wr) -> en_eb(Rd, Wr ++ ["baby mama"]);
en_eb(["ex-boyfriend" | Rd], Wr) -> en_eb(Rd, Wr ++ ["baby fatha"]);
en_eb(["ex"           | Rd], Wr) -> en_eb(Rd, Wr ++ ["baby mama"]);
en_eb(["with","me"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["and","shit"]);
en_eb(["with"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["wit"]);
en_eb(["stuff"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["shit"]);
en_eb(["gun"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["gat"]);
en_eb(["shoot"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["pop a cap"]);
en_eb(["shot"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["got wet"]);
en_eb(["relax"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["step off"]);
en_eb(["retreat"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["step off"]);
en_eb(["pardon me"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["step off"]);
en_eb(["diamonds"     | Rd], Wr) -> en_eb(Rd, Wr ++ ["ice"]);
en_eb(["teeth"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["grill"]);
en_eb(["mouth"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["grill"]);
en_eb(["head"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["dome"]);
en_eb(["punch"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["pop"]);
en_eb(["ask"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["aks"]);
en_eb(["golly"        | Rd], Wr) -> en_eb(Rd, Wr ++ ["shiiit"]);
en_eb(["obtain"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["score"]);
en_eb(["get"          | Rd], Wr) -> en_eb(Rd, Wr ++ ["score"]);
en_eb(["locally"      | Rd], Wr) -> en_eb(Rd, Wr ++ ["in da hood"]);
en_eb(["untrustworthy"| Rd], Wr) -> en_eb(Rd, Wr ++ ["skeeza"]);
en_eb(["certainly"    | Rd], Wr) -> en_eb(Rd, Wr ++ ["fo shizzy"]);
en_eb(["stupid"       | Rd], Wr) -> en_eb(Rd, Wr ++ ["whack"]);
en_eb(["slut"         | Rd], Wr) -> en_eb(Rd, Wr ++ ["chickenhead"]);
en_eb(["attractive"   | Rd], Wr) -> en_eb(Rd, Wr ++ ["hot"]);
en_eb([R|Rd], []) -> en_eb(Rd, [R]);
en_eb([R|Rd], Wr) -> en_eb(Rd, Wr ++ [R]).

load_internet_slang(Path) ->
  parse_internet_slang(util:readlines(util:relpath(
    "translate.erl", Path)), dict:new()).

parse_internet_slang([], Dict) ->
  Dict;
parse_internet_slang([Line|Rest], Dict) ->
  Line2 = util:rtrim(Line, 10),
  [Key, Val] = string:tokens(Line2, ":"),
  Val2 = util:ltrim(Val),
  Keys = string:tokens(Key, ","),
  Dict2 = lists:foldr(
    fun(K, D) ->
      dict:store(string:to_upper(K), Val2, D) end, Dict, Keys),
  %io:format("parse_internet_slang Keys=~p Val2=~p~n", [Keys, Val2]),
  parse_internet_slang(Rest, Dict2).

internet_slang([], Acc, _) ->
  Acc;
internet_slang([Word|Rest], Acc, Dict) ->
  Trans =
    case dict:find(string:to_upper(Word), Dict) of
      {ok, Val} -> Val;
      error -> Word
    end,
  internet_slang(Rest, Acc ++ [Trans], Dict).

