-module(follow).
-behaviour(cowboy_handler).
-import(twitter, [start_master/0, master/1, datafetcher/0, logoffAuto/2, getRandom/2, tweetAuto/3, logonAuto/3, followAuto/2]).
%-export([init/2]).
-compile(export_all).

init(Req0, State) ->
    

    Qs=cowboy_req:parse_qs(Req0),
    {_,Username}=lists:keyfind(<<"name">>,1,Qs),

    A=twitter:reg(Username,123), 
    io: fwrite("~p", [A]),
    Key=list_to_binary(A),
    Body=Key,

    Req=cowboy_req:http_reply(200,[],Body,
            Req0),
    {ok, Req, State}.