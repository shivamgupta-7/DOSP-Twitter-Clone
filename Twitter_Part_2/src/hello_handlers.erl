-module (hello_handlers). 
-behavior (cowboy_handler).
-import(twitter, [server_node/0,server/1,datafetcher/01]).
-export([init/2]).

init(Req0, State) ->
    A=twitter:server_node(), 
    io: fwrite("p", [A]),
    Req=cowboy_req:reply(200,#{<<"content-type">>=><<"text/plain">>},
            <<"Hello Erlang!">>,
            Req0),
    {ok, Req, State}.