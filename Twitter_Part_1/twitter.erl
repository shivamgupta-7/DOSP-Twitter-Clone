%SHIVAM GUPTA 
%CAROLINE FEDELE 

%Module Definition for the project
-module(twitter).

%All the functions are exported at once instead of calling them individually
-compile(export_all).

%------------------------------------IMPORTANT------------------------------------
%---------------------------------------------------------------------------------
%CHANGE THE SERVER NAME ALWAYS FOR A NEW SYSTEM OR SERVER FOLLOWING THE CONVENTION
%-------------------messenger + @ + USER's COMPUTER ADDRESS-----------------------
%----------------THE SERVER SHELL SHOULD ALWAYS BE NAMED MESSENGER---------------- 
server_node() ->
    'messenger@DESKTOP-KHTE3K0'.

%Server Contanins the entire list of all the users in the system
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List);
        {From, follow_to, To}->
            server_follow(From, To, User_List),
            server(User_List);
        {user_list,Pid}->
            io:fwrite("~p",Pid);
        %register new user
        {register, From, Msg, UserName, Password}->
            Temp=#{UserName => Password},

            Userdat = persistent_term:get(userdata),

            NewUserdat = maps:merge(Temp, Userdat),

            persistent_term:put(userdata, NewUserdat),

            UserList=persistent_term:get(userdata),

            Temp_followers= #{UserName => []},

            Followers=persistent_term:get(followers),

            NewFollowersmp = maps:merge(Followers,Temp_followers),

            persistent_term:put(followers, NewFollowersmp),

            Fol=persistent_term:get(followers),

            Temp_following = #{UserName =>[]},

            Following = persistent_term:get(following),

            NewFollowingmp = maps:merge(Following, Temp_following),

            persistent_term:put(following, NewFollowingmp),

            Temp_tweet = #{UserName => []},

            Tweetmp = persistent_term:get(tweets),

            NewTweetmp = maps:merge(Tweetmp, Temp_tweet),

            persistent_term:put(tweets, NewTweetmp),

            Temp_lsttweet = #{UserName => ""},

            Lasttweetmp = persistent_term:get(lastmsg),

            NewLastTweetmp = maps:merge(Lasttweetmp, Temp_lsttweet),

            persistent_term:put(lastmsg, NewLastTweetmp),

            io:fwrite("~p",[NewLastTweetmp]);


        {update, Followmp}->
            persistent_term:put(followers, Followmp);

        {tweetupd, Tweetmap, Alltweets} ->
            io:fwrite("~p",[Tweetmap]),
            persistent_term:put(alltweets, Alltweets),
            persistent_term:put(tweets, Tweetmap);
        {updlstmsg, Message, Name}->
            Lastmsg=persistent_term:get(lastmsg),
            Lastmsg2=maps:update(Name, Message, Lastmsg),
            persistent_term:put(lastmsg, Lastmsg2)
    end,
    server(User_List).

% ------------------------- Function for the Server to Fetch Data for Use
datafetcher()->
    receive
        {user_list, From}->
            Userdat=persistent_term:get(userdata),
            From ! {Userdat};
        {followmap, From}->
            Followmp=persistent_term:get(followers),
            From ! {Followmp};
        {tweetmap, From}->
            Tweetmp=persistent_term:get(tweets),
            Alltweets=persistent_term:get(alltweets),
            From ! {Tweetmp, Alltweets};
        {lstmsg, From}->
            Lastmsg = persistent_term:get(lastmsg),
            From ! {Lastmsg}
    end,
    datafetcher().


%---------------------------------------------SIMULATOR CODE START ----------------------------
for_reg(0)->
    ok;
for_reg(N)->
    reg(N,"1"),
    for_reg(N-1).

for_log(0,Max)->
    io:fwrite("\n"),
    ok;
for_log(N,Max)->

    logonAuto(N,N,Max),
    for_log(N-1,Max).

for_follow(0,Max)->
    io:fwrite("\n"),
    ok;
for_follow(N,Max)->
    User1 = rand:uniform(N),
    User2 = getRandom(User1, Max),
    persistent_term:put(user, User1),

    followAuto(User1, User2),
    tweetAuto(User2,"Hello",25),

    for_follow(N-1, Max).

for_off(0)->
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000,
    U2 = Time2 * 1000,
    io:format("Code time=~p (~p)~n", [U2, U1]),
    ok;

for_off(N)->
    logoffAuto(N,N),
    for_off(N-1).

logoffAuto(N,N)->
    {fetch, server_node()} ! {user_list, self()},

    receive 
        {Dat}->
            Userdata=Dat 
    end,
    maps:remove(N, Userdata),
    io:fwrite("@~p has logged off\n",[N]),
    X=maps:size(Userdata),
    if 
        X==0 ->
            exit(bas);
        true ->
            ok
    end.

getRandom(User1, N) ->
    User2 = rand:uniform(N),
    if 
        User1==User2 ->
            getRandom(User1, N);
        true ->
            User2
    end.

tweetAuto(User,Message,0)->
    FollwersMap = persistent_term:get(folAuto),
    {fetch, server_node()} ! {tweetmap, self()},
    receive 
        {Tweetmp, Alt}->
            Tweet = Tweetmp,
            Alltweets = Alt 
    end,

    Tweetsmap=persistent_term:get(tweetsAuto),

    Newalltweets=lists:append(Alltweets, [Message]),

    List = maps:get(User, FollwersMap),

    Tweetlist = maps:get(User, Tweetsmap),

    Tweetlist2 = lists:append(Tweetlist, [Message]),

    Tweetmp1 = maps:update(User, Tweetlist2, Tweetsmap),

    {twitter, server_node()} ! {tweetupd, Tweetmp1, Newalltweets},

    lists:foreach(
        fun(Elem) ->
            io:fwrite("To @~p From: @~p Tweet: ~p~n", [Elem, User, Message])
        
        end,
        List
    );

tweetAuto(User, Message,N)->
    FollwersMap = persistent_term:get(folAuto),

    {fetch, server_node()} ! {tweetmap, self()},
    receive 
        {Tweetmp, Alt}->
            Tweet = Tweetmp,
            Alltweets = Alt 
    end,

    Tweetsmap=persistent_term:get(tweetsAuto),

    Newalltweets=lists:append(Alltweets, [Message]),

    List = maps:get(User, FollwersMap),

    Tweetlist = maps:get(User, Tweetsmap),

    Tweetlist2 = lists:append(Tweetlist, [Message]),

    Tweetmp1 = maps:update(User, Tweetlist2, Tweetsmap),

    {twitter, server_node()} ! {tweetupd, Tweetmp1, Newalltweets},

    lists:foreach(
        fun(Elem) ->
            io:fwrite("To @~p From: @~p Tweet: ~p~n", [Elem, User, Message])
        
        end,
        List
    ),
    tweetAuto(User,Message,N-1).

logonAuto(Name, _Password, Max)->
        io:fwrite("@~p has logged on\n", [Name]),
        if 
            Name==Max ->
                {fetch, server_node()} ! {followmap, self()},
                receive 
                    {Fol} ->
                        FollowersMap = Fol 

                end,
                {fetch, server_node()} ! {tweetmap, self()},
                receive 
                    {Tweet, All} ->
                        TweetsMap = Tweet 
                end, 
                {fetch, server_node()} ! {lstmsg, self()},
                receive 
                    {Last} ->
                        LastMap=Last 
                end; 
            true ->
                FollowersMap = persistent_term:get(folAuto),
                TweetsMap = persistent_term:get(tweetsAuto),
                LastMap = persistent_term:get(lastAuto)
        end,

        FollowersMap2= #{Name => []},
        FollowersMap3=maps:merge(FollowersMap2, FollowersMap),

        persistent_term:put(folAuto, FollowersMap3),


        Temp_tweet = #{Name => []},

        NewTweetmp=maps:merge(TweetsMap, Temp_tweet),

        persistent_term:put(tweetsAuto, NewTweetmp),

        Temp_lsttweet = #{Name => ""},

        NewLastTweetmp = maps:merge(LastMap, Temp_lsttweet),

        persistent_term:put(lastAuto, NewLastTweetmp).

followAuto(User1, User2)->







    FollowersMap = persistent_term:get(folAuto),

    List = maps:get(User2, FollowersMap),
    List2 = lists:append(List, [User1]),
    FollowersMap2 = maps:update(User2, List2, FollowersMap),
    FollowersMap3 = maps:merge(FollowersMap, FollowersMap2),

    persistent_term:put(folAuto, FollowersMap3).


simulator(N) ->
    statistics(runtime),
    statistics(wall_clock),
    for_reg(N),
    for_log(N,N),
    Half = N-50,
    for_follow(Half, N),

    for_off(N).


%%%%%%%%%%%%------------------SIMULATOR CODE END------------------------------------------
%%%
%%%
%%% Start the server
start_server() ->
    Msg = "shivamgupta",
    persistent_term:put(n,Msg),

    Userlist = [],
    persistent_term:put(userlist, Userlist),


    Userdata=#{},
    persistent_term:put(userdata, Userdata),


    Usertopid=#{},
    persistent_term:put(usertopid, Usertopid),


    Pidtouser=#{},
    persistent_term:put(pidtouser,Pidtouser),


    TweetsMap=#{},
    persistent_term:put(tweets,TweetsMap),


    FollowersMap=#{},
    persistent_term:put(followers, FollowersMap),


    FollowingMap=#{},
    persistent_term:put(following, FollowingMap),

    Alltweets = [],
    persistent_term:put(alltweets, Alltweets),

    Lastmsg=#{},
    persistent_term:put(lastmsg, Lastmsg),

    register(twitter, spawn(twitter, server, [[]])),
    register(fetch, spawn(twitter, datafetcher, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->

            From ! {twitter, stop, user_exists_at_other_node},  %reject logon
            User_List;
        false ->
            From ! {twitter, logged_on},
           
            [{From, Name} | User_List]        %add user to the list
    end.

%%% Server deletes a user from the user list
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

server_follow(From, To, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {twitter, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_follow(From, Name, To, User_List)
    end.

server_follow(From, Name, To, User_List) ->
   
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {twitter, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {follow_from, Name},
            From ! {twitter, sent}
    end.

%%% Server transfers a message between user
server_transfer(From, To, Message, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {twitter, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {twitter, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message}, 
            From ! {twitter, sent} 
    end.

%%% User Commands

reg(Username, Password) ->
    Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Password))]),
    io:fwrite(" ~p", [Hash]),
    {twitter, server_node()} ! {register, self(), "reg", Username,Hash}.

logon(Name, Password) ->
   case whereis(mess_client) of 
        undefined ->
            % encryption
            %Pass = Password ++ "", 
            Hash1 = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Password))]),
            {fetch, server_node()} ! {user_list, self()},
            receive 
                {Dat} ->
                    Userdata = Dat,
                    io:fwrite("~p", [Dat])
            end,
            Booluser=maps:is_key(Name, Userdata),
            if 
                Booluser == true ->
                    Hashed_pass = maps:get(Name, Userdata),
                    % IsEqual = equal(Hashed_pass, Hash1),
                    % io:fwrite(IsEqual),
                    if 
                        Hash1 == Hashed_pass ->
                            register(
                                mess_client, 
                                spawn(twitter, client, [server_node(), Name, Name])
                            );
                        true ->
                            io:fwrite("Joined")
                    end;
                true ->
                    io:fwrite("Joined")
            end;
        _ -> 
            already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

follow(ToName) ->
    case whereis(mess_client) of 
        undefined -> 
            not_logged_on;
        _ ->
            mess_client ! {follow_to, ToName},
            ok
    end. 

message(ToName, Message) ->

    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 
            mess_client ! {message_to, ToName, Message},
            ok
end.

tweet(Message) ->

    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> 
            {fetch, server_node()} ! {followmap, self()},
            receive 
                {Fol} ->
                        FollowersMap = Fol
            end,

            mess_client ! {fetchmyname, self()},

            receive 
                {Myname} ->
                    User = Myname
            end,
                
            {fetch, server_node()} ! {tweetmap, self()},
            receive 
                {Tweetmp, Alt} ->
                        TweetsMap = Tweetmp,
                        Alltweets =Alt
                end, 



                Newalltweets=lists:append(Alltweets, [Message]),

                List = maps:get(User, FollowersMap),

                Tweetlist = maps:get(User, TweetsMap),

                Tweetlist2 = lists:append(Tweetlist, [Message]),

                Tweetmp1 = maps:update(User, Tweetlist2, TweetsMap),

                io:fwrite("~p",[Tweetmp1]),

                {twitter, server_node()} ! {tweetupd, Tweetmp1, Newalltweets},

                lists:foreach(
                    fun(Elem) ->
                        io:fwrite("Elem ~p~n", [Elem]),
                        mess_client ! {message_to, Elem, Message}
                    end,
                    List
                )
        end.

search(Query)->
    {fetch, server_node()} ! {tweetmap, self()},
    receive
        {_Twtmp, Alt} ->
            Alltweets = Alt
    end, 

    lists:foreach(
        fun(S) ->
            Bool = string:str(S, Query) > 0,
            if 
                Bool == true ->
                    io:fwrite("Result: ~p~n",[S]);
                true ->
                    ok 
            end 
        end,
        Alltweets
    ).

mention() ->
    {fetch, server_node()} ! {tweetmap, self()},
    receive 
        {_Twtmp, Alt} ->
            Alltweets = Alt 
    end, 

    mess_client ! {fetchmyname, self()},
    receive 
        {Myname} ->
            User = Myname
        end, 



    Query="@" ++ atom_to_list(User),
    lists:foreach(
        fun(S) ->
            Bool = string:str(S, Query) > 0,
            if 
                Bool == true ->
                    io:fwrite("Result: ~p~n",[S]);
                true ->
                    no_mentions_yet 
            end 
        end,
        Alltweets
    ).

retweet() ->
    {fetch, server_node()} ! {lstmsg, self()},
    receive 
        {Lst} ->
            Lastmsg = Lst
        end,

        mess_client ! {fetchmyname, self()},
        receive 
            {Nm} ->
                Name = Nm 
        end,

        Msg = maps:get(Name, Lastmsg),
        Retweet = "Re:" ++ Msg,
        io:fwrite("~p -> ~p -> ~p",[Retweet, Name, Lastmsg]),

        tweet(Retweet). 

%%% The client process which runs on each server node
client(Server_Node, Name, Myname) ->
    {twitter, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node,Myname).

client(Server_Node, Myname) ->
    io:fwrite("~p~n",[Myname]),

    receive
        logoff ->
            {twitter, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {twitter, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message]),

            {twitter, Server_Node} ! {updlstmsg, Message, Myname};
        {follow_to, ToName} -> 
            {twitter, Server_Node} ! {self(), follow_to, ToName},
            await_result();
        {follow_from, FromName} ->
            {fetch, Server_Node} ! {followmap, self()},
            receive 
                {Fol} ->
                    FollowersMap = Fol 
            end,

            io:fwrite("~p", [FollowersMap]),
            List = maps:get(Myname, FollowersMap),
            List2 = lists:append(List, [FromName]),
            FollowersMap2 = maps:update(Myname, List2, FollowersMap),
            FollowersMap3 = maps:merge(FollowersMap, FollowersMap2),
            {twitter, Server_Node} ! {update, FollowersMap3};


        {fetchmyname, From} ->
            From ! {Myname}
    end,
    client(Server_Node, Myname).

%%% wait for a response from the server
await_result() ->
    receive

        {twitter, stop, Why} -> % Stop the client 
            io:format("~p~n", [Why]),
            exit(normal);

        {twitter, What} ->  % Normal response
            io:format("~p~n", [What])
    end.