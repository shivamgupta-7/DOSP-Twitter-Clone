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
    'twitter@127.0.0.1'.

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
        {register, From, message, UserName, Password}->
            Temporary=#{UserName => Password},

            Userdat = persistent_term:get(userdata),

            Clientdata = maps:merge(Temporary, Userdat),

            persistent_term:put(userdata, Clientdata),

            UserList=persistent_term:get(userdata),

            Temporary_followers= #{UserName => []},

            Followers=persistent_term:get(followers),

            ClientMap = maps:merge(Followers,Temporary_followers),

            persistent_term:put(followers, ClientMap),

            Fol=persistent_term:get(followers),

            Temporary_following = #{UserName =>[]},

            Following = persistent_term:get(following),

            NewFollowingmp = maps:merge(Following, Temporary_following),

            persistent_term:put(following, NewFollowingmp),

            Temporary_tweet = #{UserName => []},

            Tweetjson = persistent_term:get(tweets),

            NewTweetjson = maps:merge(Tweetjson, Temporary_tweet),

            persistent_term:put(tweets, NewTweetjson),

            Temporary_lsttweet = #{UserName => ""},

            LastTweetjson = persistent_term:get(lastmessage),

            NewLastTweetjson = maps:merge(LastTweetjson, Temporary_lsttweet),

            persistent_term:put(lastmessage, NewLastTweetjson),

            io:fwrite("~p",[NewLastTweetjson]);


        {update, Followmp}->
            persistent_term:put(followers, Followmp);

        {tweetupd, Tweetmap, Alltweets} ->
            io:fwrite("~p",[Tweetmap]),
            persistent_term:put(alltweets, Alltweets),
            persistent_term:put(tweets, Tweetmap);
        {updlstmessage, Message, Name}->
            Lastmessage=persistent_term:get(lastmessage),
            Lastmessage2=maps:update(Name, Message, Lastmessage),
            persistent_term:put(lastmessage, Lastmessage2)
    end,
    server(User_List).

% ------------------------- Function for the Server to Fetch Data for Use
tweetdata()->
    receive
        {user_list, From}->
            Userdat=persistent_term:get(userdata),
            From ! {Userdat};
        {followmap, From}->
            Followmp=persistent_term:get(followers),
            From ! {Followmp};
        {tweetmap, From}->
            Tweetjson=persistent_term:get(tweets),
            Alltweets=persistent_term:get(alltweets),
            From ! {Tweetjson, Alltweets};
        {lstmessage, From}->
            Lastmessage = persistent_term:get(lastmessage),
            From ! {Lastmessage}
    end,
    tweetdata().


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
        {Tweetjson, Alt}->
            Tweet = Tweetjson,
            Alltweets = Alt 
    end,

    Tweetsmap=persistent_term:get(tweetsAuto),

    Newalltweets=lists:append(Alltweets, [Message]),

    List = maps:get(User, FollwersMap),

    Tweetlist = maps:get(User, Tweetsmap),

    Tweetlist2 = lists:append(Tweetlist, [Message]),

    Tweetjson1 = maps:update(User, Tweetlist2, Tweetsmap),

    {twitter, server_node()} ! {tweetupd, Tweetjson1, Newalltweets},

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
        {Tweetjson, Alt}->
            Tweet = Tweetjson,
            Alltweets = Alt 
    end,

    Tweetsmap=persistent_term:get(tweetsAuto),

    Newalltweets=lists:append(Alltweets, [Message]),

    List = maps:get(User, FollwersMap),

    Tweetlist = maps:get(User, Tweetsmap),

    Tweetlist2 = lists:append(Tweetlist, [Message]),

    Tweetjson1 = maps:update(User, Tweetlist2, Tweetsmap),

    {twitter, server_node()} ! {tweetupd, Tweetjson1, Newalltweets},

    lists:foreach(
        fun(Elem) ->
            io:fwrite("To @~p From: @~p Tweet: ~p~n", [Elem, User, Message])
        
        end,
        List
    ),
    tweetAuto(User,Message,N-1).
%-------------------------------------We start with a random node from finger table to move to find key
random_loop(Node_id, Node_list, NumRequests) ->

    if
        NumRequests == full ->
            Random_node = lists:nth(rand:uniform(length(Node_list)), Node_list),
            if
                Random_node == Node_id ->
                    random_loop(Node_id, Node_list, NumRequests);
                true ->
                    Random_node
            end;

        NumRequests == line ->
            Neighbor_Map = persistent_term:get(neighbor),
            Neighbor_List = maps:get(Node_id, Neighbor_Map),
            Random_node = lists:nth(rand:uniform(length(Neighbor_List)), Neighbor_List),
            if
                Random_node == Node_id ->
                    random_loop(Node_id, Node_list, NumRequests);
                true ->
                    Random_node
            end;
        true ->
            ok
    end.

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
                {fetch, server_node()} ! {lstmessage, self()},
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


        Temporary_tweet = #{Name => []},

        NewTweetjson=maps:merge(TweetsMap, Temporary_tweet),

        persistent_term:put(tweetsAuto, NewTweetjson),

        Temporary_lsttweet = #{Name => ""},

        NewLastTweetjson = maps:merge(LastMap, Temporary_lsttweet),

        persistent_term:put(lastAuto, NewLastTweetjson).

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

%-------------------------------------------------Joing of New Nodes to the List---------------------------------------------
joining(_NumNodes) ->
    Fin_List = persistent_term:get(fin),
    RMap = persistent_term:get(rmap),
    io:fwrite("~p~n", [Fin_List]),
    Bool = maps:find(self(), RMap),
    Check = lists:member(self(), Fin_List),

    if
        Bool == {ok, true} ->
            if
                Check == false ->
                    LL = persistent_term:get(list),
                    Weights_map = persistent_term:get(weights),
                    {Sum, Weight} = maps:get(self(), Weights_map),
                    Rand = random_loop(self(), LL, 10),
                    Rand ! {self(), Sum, Weight};
                true ->
                    ok
            end;
        true ->
            ok
    end,

    sid ! {self(), "self"},
    receive
        {_Id, initial} ->
            Weights_NewM3 = persistent_term:get(weights),
            {S2, W2} = maps:get(self(), Weights_NewM3),
            NewS = S2 / 2,
            NewW = W2 / 2,
            Weights_NewM2 = maps:update(self(), {NewS, NewW}, Weights_NewM3),
            persistent_term:put(weights, Weights_NewM2),
            RNewM2 = persistent_term:get(rmap),
            R = maps:update(self(), true, RNewM2),
            persistent_term:put(rmap, R),

            Random_node = random_loop(self(), persistent_term:get(list), 10),
            Random_node ! {self(), NewS, NewW};

        {_Pid, S, W} ->
            Weights_NewM3 = persistent_term:get(weights),
            {Pid_Sum, Pid_Weight} = maps:get(self(), Weights_NewM3),
            NewSum = Pid_Sum + S,
            NewWeight = Pid_Weight + W,

            if
                W /= 0.0 ->
                    Change = abs(NewSum / NewWeight - S / W),
                    Delta = math:pow(10, -10),
                    if
                        Change < Delta ->
                            Node_map = persistent_term:get(map),
                            TermRound = maps:get(self(), Node_map),
                            Node_NewM2 = maps:update(self(), TermRound + 1, Node_map),
                            persistent_term:put(map, Node_NewM2);
                        true ->
                            Node_map = persistent_term:get(map),
                            Node_NewM2 = maps:update(self(), 0, Node_map),
                            persistent_term:put(map, Node_NewM2)
                    end,
                    Node_NewM3 = persistent_term:get(map),
                    TermRound2 = maps:get(self(), Node_NewM3),
                    if
                        TermRound2 == 3 ->
                            Fin_List = persistent_term:get(fin),
                            Fin_List2 = lists:append(Fin_List, [self()]),
                            persistent_term:put(fin, Fin_List2);
                        true ->
                            ok
                    end;
                true ->
                    ok
            end,

            Weights_NewM2 = maps:update(self(), {NewSum, NewWeight}, Weights_NewM3),
            persistent_term:put(weights, Weights_NewM2),
            RNewM2 = persistent_term:get(rmap),
            R = maps:update(self(), true, RNewM2),
            persistent_term:put(rmap, R),

            Random_node = random_loop(self(), persistent_term:get(list), 10),
            Random_node ! {self(), NewSum / 2, NewWeight / 2};


        {_Pid} ->
            F_List = persistent_term:get(fin),
            X = length(F_List),
            if
                X == ring ->
                    Neighbor = persistent_term:get(neighbor),
                    Is_key = maps:is_key(self(), Neighbor),

                    Finlen = length(F_List),

                    if
                        Is_key == true ->
                            if
                                Finlen =/= 0 ->
                                    N_list = maps:get(self(), Neighbor),
                                    Y = length(N_list);
                                % io:fwrite("~p\n",[K]);
                                true ->
                                    d
                            end;
                        true ->
                            ok
                    end;
                true ->
                    ok
            end,

            if
                X == 10 ->
                    io:format("Finished List: ~p~n", [F_List]),
                    lastid ! {programend},
                    exit(bas);
                true ->
                    joining(10)
            end,

            Finished_List = persistent_term:get(fin),
            X = length(Finished_List),
            if
                X == 10 ->
                    lastid ! {programend},
                    exit(bas);
                true ->
                    joining(10)
            end
    end,
    joining(10).


%%%%%%%%%%%%------------------SIMULATOR CODE END------------------------------------------
%%%
%%%
%%% Start the server
start_server() ->
    message = "shivamgupta",
    persistent_term:put(n,message),

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

    Lastmessage=#{},
    persistent_term:put(lastmessage, Lastmessage),

    register(twitter, spawn(twitter, server, [[]])),
    register(fetch, spawn(twitter, tweetdata, [])).

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
    {twitter, server_node()} ! {register, self(), "reg", Username, Password}.

logon(Name, Password) ->
   case whereis(mess_client) of 
        undefined ->
            % encryption
            %Pass = Password ++ "", 
            Hash1 = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Password))]),
             io:fwrite(" ~p", [Hash1]),
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
                        Password == Hashed_pass ->
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

%---------------------------------------------------Setting Up the Chord Function for Traversal and Ring Formation --------------------------
setupChord(0, _Idx, _Limit) ->
    mid ! {"done"},
    ok;
setupChord(Num, Index, Limit) ->
    Node_list = persistent_term:get(list),
    Pid = lists:nth(Index, Node_list),

    PlaneSize = persistent_term:get(plane),
    RowSize = persistent_term:get(row),

    East = east(Index, RowSize),
    West = west(Index, RowSize),
    North = north(Index, RowSize, PlaneSize),
    South = south(Index, RowSize, PlaneSize),

    if
        East /= -1 ->
            Neighbor_Map = persistent_term:get(neighbor),
            Neighbors = maps:get(Pid, Neighbor_Map),
            EastNeighbor = lists:nth(East, Node_list),
            N = lists:append(Neighbors, [EastNeighbor]),
            Neighbor_map2 = maps:update(Pid, N, Neighbor_Map),
            persistent_term:put(neighbor, Neighbor_map2);
        true ->
            ok
    end,

    if
        West /= -1 ->
            Neighbor_Map5 = persistent_term:get(neighbor),
            Neighbors5 = maps:get(Pid, Neighbor_Map5),
            WestNeighbor = lists:nth(West, Node_list),
            N5 = lists:append(Neighbors5, [WestNeighbor]),
            Neighbor_map25 = maps:update(Pid, N5, Neighbor_Map5),
            persistent_term:put(neighbor, Neighbor_map25);
        true ->
            ok
    end,

    if
        North /= -1 ->
            Neighbor_Map3 = persistent_term:get(neighbor),
            Neighbors3 = maps:get(Pid, Neighbor_Map3),
            NorthNeighbor = lists:nth(North, Node_list),
            N3 = lists:append(Neighbors3, [NorthNeighbor]),
            Neighbor_map23 = maps:update(Pid, N3, Neighbor_Map3),
            persistent_term:put(neighbor, Neighbor_map23);
        true ->
            ok
    end,

    if
        South /= -1 ->
            Neighbor_Map4 = persistent_term:get(neighbor),
            Neighbors4 = maps:get(Pid, Neighbor_Map4),
            SouthNeighbor = lists:nth(South, Node_list),
            N4 = lists:append(Neighbors4, [SouthNeighbor]),
            Neighbor_map24 = maps:update(Pid, N4, Neighbor_Map4),
            persistent_term:put(neighbor, Neighbor_map24);
        true ->
            ok
    end,
   
            setupChord(Num - 1, Index + 1, Limit).
 
%-------------------------------------North, South, East, West Used by Setup Chord to find the place the new node is to be added and updation 
%-------------------------------------of the table and list for the new node--------------------------------------------------------------
east(Index, RowSize) ->
    Mod = math:fmod(Index, RowSize),
    if
        Mod == 0 ->
            -1;
        true ->
            Index + 1
    end.

west(Index, RowSize) ->
    Mod = math:fmod(Index, RowSize),
    if
        Mod == 1 ->
            -1;
        true ->
            Index - 1
    end.

north(Index, RowSize, PlaneSize) ->
    Mod = math:fmod(Index, PlaneSize),
    if
        Mod >= 1 andalso Mod =< RowSize ->
            -1;
        true ->
            Index - RowSize
    end.

south(Index, RowSize, PlaneSize) ->
    Mod = math:fmod(Index, PlaneSize),
    Size = PlaneSize - RowSize + 1,
    if
        Mod >= Size andalso Mod =< PlaneSize ->
            -1;
        Mod == 0 ->
            -1;
        true ->
            Index + RowSize
    end.

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
                {Tweetjson, Alt} ->
                        TweetsMap = Tweetjson,
                        Alltweets =Alt
                end, 



                Newalltweets=lists:append(Alltweets, [Message]),

                List = maps:get(User, FollowersMap),

                Tweetlist = maps:get(User, TweetsMap),

                Tweetlist2 = lists:append(Tweetlist, [Message]),

                Tweetjson1 = maps:update(User, Tweetlist2, TweetsMap),

                io:fwrite("~p",[Tweetjson1]),

                {twitter, server_node()} ! {tweetupd, Tweetjson1, Newalltweets},

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
    {fetch, server_node()} ! {lstmessage, self()},
    receive 
        {Lst} ->
            Lastmessage = Lst
        end,

        mess_client ! {fetchmyname, self()},
        receive 
            {Nm} ->
                Name = Nm 
        end,

        message = maps:get(Name, Lastmessage),
        Retweet = "Re:" ++ message,
        io:fwrite("~p -> ~p -> ~p",[Retweet, Name, Lastmessage]),

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

            {twitter, Server_Node} ! {updlstmessage, Message, Myname};
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