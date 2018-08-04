-module(fnoxc_http).

-export([get/2,
	 post/3,
	 put/3,
	 delete/2]).

get(Url, Opts) ->
    Headers = build_header(Opts),
    {ok, {_, _, Body}} = httpc:request(get, {Url, Headers}, [], []),
    Body.

post(Url, Body, Opts) ->
    Headers = build_header(Opts),
    {ok, {_, _, Body}} = httpc:request(post, {Url, Headers, "application/json", Body}, [], []),
    Body.

put(Url, Body, Opts) ->
    Headers = build_header(Opts),
    {ok, {_, _, Body}} = httpc:request(put, {Url, Headers, "application/json", Body}, [], []),
    Body.

delete(Url, Opts) ->
    Headers = build_header(Opts),
    {ok, {_, _, Body}} = httpc:request(delete, {Url, Headers}, [], []),
    Body.

build_header(Opts) ->
    Token = maps:get(access_token, Opts),
    ClientSecret = maps:get(client_secret, Opts),
    [{"Access-Token", Token},
     {"Client-Secret", ClientSecret},
     {"Content-Type", "application/json"},
     {"Accept", "application/json"}].
