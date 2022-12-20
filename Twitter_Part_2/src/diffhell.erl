-module(diffhell).
-import(twitter,[server/1]).
-export([auth/0]).

auth() ->
    crypto:start(),
    spawn(fun () -> {ok, Sock} = gen_tcp:listen(12321, [binary, {packet, raw}]),
    echo_loop(Sock)
    end).

echo_loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    io:format("Got connection: ~p~n", [Conn]),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    echo_loop(Sock).

handle(Conn) ->
    receive
        {tcp, Conn, Yc} ->
            Xs = crypto:strong_rand_bytes(64),
            Ys = crypto:mod_pow(g(),Xs,p()),
            S = crypto:mod_pow(Yc, Xs, p()),

            AESKey = crypto:hash(sha256, S),

            gen_tcp:send(Conn, Ys),%KeyCert),
            handle(Conn);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.

p() ->
    16#ffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece65381ffffffffffffffff.

g() ->
    2.