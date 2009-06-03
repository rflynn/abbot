% ex: set ts=2 noet:
% $Id$

-module(cgi).
-author("pizza@parseerror.com").
-export([
		test/0,
		entity_decode/1,
		url_encode/1
	]).
-import(test).

test() ->
	test:unit(cgi,
		[
			{ url_encode, test_url_encode() }
		]).

entity_decode(En) -> entity_decode(En, []).
entity_decode([], Un) -> Un;
entity_decode("&nbsp;"  ++ De, Un) -> entity_decode(De, Un ++ " ");
entity_decode("&lt;"    ++ De, Un) -> entity_decode(De, Un ++ "<");
entity_decode("&gt;"    ++ De, Un) -> entity_decode(De, Un ++ ">");
entity_decode("&amp;"   ++ De, Un) -> entity_decode(De, Un ++ "&");
entity_decode("&mdash;" ++ De, Un) -> entity_decode(De, Un ++ "--");
entity_decode("&copy;"  ++ De, Un) -> entity_decode(De, Un ++ "(c)");
entity_decode("&quot;"  ++ De, Un) -> entity_decode(De, Un ++ "\'");
entity_decode("&ndash;" ++ De, Un) -> entity_decode(De, Un ++ "-");
entity_decode([D|De],          Un) -> entity_decode(De, Un ++ [D]).

url_encode(Un) -> url_encode(Un, []).
url_encode([], En) -> En;
url_encode(" " ++ De, En) -> url_encode(De, En ++ "+");
url_encode("&" ++ De, En) -> url_encode(De, En ++ "&amp;");
url_encode("=" ++ De, En) -> url_encode(De, En ++ "=");
url_encode("/" ++ De, En) -> url_encode(De, En ++ "/");
url_encode([D|De],    En) -> url_encode(De, En ++ [D]).

test_url_encode() ->
	[
		{ [ "" ],	"" },
		{ [ " " ],	"+" },
		{ [ "a" ],	"a" },
		{ [ "ab" ],	"ab" },
		{ [ "a b" ],	"a+b" },
		{ [ "a & b" ],	"a+&amp;+b" },
		{ [ "hello&there" ],	"hello&amp;there" }
	].

