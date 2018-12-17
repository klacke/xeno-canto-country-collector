-module(xeno).
-compile(export_all).

-include("yaws_api.hrl").


-define(liof(Fmt, Args),
        io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
%-define(liof(Fmt, Args), ok).

-define(cerror_msg(Fmt, Args),
        error_logger:error_msg("~w:~w: " ++ Fmt ++ "~p\n",
                               [?MODULE, ?LINE | Args] ++
                                   [erlang:get_stacktrace()])).



test() ->
    parse("t.xls").

gen_checklist_tab() ->
    All = read_checklists(),
    T = ets:new(birds, [set]),
    lists:foreach(fun({_Country, Birds}) ->
                          lists:foreach(fun({B, Latin}) ->
                                                ets:insert(T, {B, Latin})
                                        end, Birds)
                  end, All),
    T.


read_checklists() ->
    Dir = "/Users/klacke/checklists",
    {ok, L} = file:list_dir(Dir),
    _All = lists:map(fun(File) ->
                      io:format("File = ~p~n", [File]),
                      parse(Dir ++ "/" ++ File)
                    end,  L).

generate_country_checklists() ->
    All = read_checklists(),
    file:write_file("checklists.bin", term_to_binary(All)).



parse(File) ->
    {ok, Bin} = file:read_file(File),
    P = mochiweb_html:parse(binary_to_list(Bin)),
    {_Head, _, Parts} = P,
    Body = hd(tl(Parts)),
    {_Body, _Id, BodyParts} = Body,
    H1 =  hd(BodyParts),
    %io:format("H1 = ~p~n", [H1]),
    {_, _, Hparts} = H1,
    [{_, _, [Text|_]}|_] = Hparts,
    Country = h1_strip(binary_to_list(Text)),
    io:format("Country = ~p~n", [Country]),

    {ok, Tab} = find_table(BodyParts),
    [_First | Rows] = Tab,
    {Country, lists:zf(fun(R) ->
                               parse_row(R)
                       end, Rows)}.


parse_row(R) ->
    {_TR, _, TD} = R,
    [_, Name, Latin |_] = TD,
    case get_name(Name) of
        "Race" ->
             false;
        N ->
            {true, {N, get_name(Latin)}}
    end.

get_name({_TD, _, [{_, _, [N|_]} |_]}) ->
    N;
get_name(_) ->
    "Race".

find_table({<<"table">>, _, Tab}) ->
    {ok, Tab};
find_table({_, _, List}) when is_list(List) ->
    find_table(List);
find_table([H|T])->
    %io:format("Checking ~p~n", [H]),
    case find_table(H) of
        {ok, Tab} ->
            {ok, Tab};
        false ->
            find_table(T)
    end;
find_table(_) ->
    false.

h1_strip("Travel list" ++ Rest) ->
    strip_until_upcase(Rest).

strip_until_upcase([H|T] ) when $A =< H, H=<$Z ->
    [H|T];
strip_until_upcase([_|T]) ->
    strip_until_upcase(T).


all() ->
    Tab = gen_checklist_tab(),
    lists:foreach(fun({N,L}) ->
                          dl_xeno({N, L})
                  end, ets:tab2list(Tab)).


-record(item, {
          name,
          file,
          sound,
          rating,
          time,
          country}).



dl_xeno() ->
    {Name, Latin} = {<<"Black-capped Parakeet">>,<<"Pyrrhura rupicola">>},
    dl_xeno({Name, Latin}).

dl_xeno({Name, Latin}) ->
    io:format("Doing ~p~n", [Name]),
    application:start(asn1),
    application:start(public_key),
    application:start(crypto),
    application:start(ssl),
    ibrowse:start(),
    P = xeno_entry({Name, Latin}),

    {_Head, _, Parts} = P,
    Body = hd(tl(Parts)),
    {_Body, _Id, BodyParts} = Body,
    [_Header | Divs] = BodyParts,
    Trs = lists:filter(
            fun(Tr) ->
                    case element(1, Tr) of
                        <<"tr">> ->
                            true;
                        _ ->
                            false
                    end
            end, Divs),

    %% can be [] !!!
    Calls = lists:zf(fun(Tr) ->
                     case catch collect(Tr) of
                         {'EXIT', _Err} ->
                                                %?cerror_msg("~p~n", [Err]),
                                                %?liof("DROP \n~p~n",[Tr]),
                             false;
                         Ret ->
                             {true, Ret}
                     end
                     end,Trs),
    Chosen = choose(Calls),
    ok = file:make_dir("calls/" ++ binary_to_list(Name)),
    lists:foreach(fun(C) ->
                          File = binary_to_list(C#item.file),
                          Url = "https:" ++ File,
                          io:format(" GET ~p~n", [Url]),
                          case ibrowse:send_req(Url, [], get) of
                              {ok, "200", _, Body2} ->
                                  Fname = lists:last(
                                            string:tokens(
                                              unencode(File),"/")),
                                  FF = "calls/" ++ binary_to_list(Name) ++
                                                      "/" ++ Fname,
                                  io:format("Write to ~p~n", [FF]),
                                  ok = file:write_file(FF, Body2);
                              _Err ->
                                  io:format("ERR ~p~n", [_Err])
                          end
                  end,
                  Chosen).

unencode([$%, $2, $0 | Tail]) ->
    [$\s | unencode(Tail)];
unencode([H|T]) ->
    [H | unencode(T)];
unencode([]) ->
    [].


xeno_entry({N, L}) ->
    Url = io_lib:format(
               "https://www.xeno-canto.org/explore?query=~s&pg=1",
            [encode(binary_to_list(N))]),
    wg(N, L, Url).

wg(_Name, _Latin, Url) ->
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Attrs, Body} ->
            mochiweb_html:parse(Body);
        _E->
            _E
    end.

encode([$\s|T]) ->
    [$+|encode(T)];
encode([H|T]) ->
    [H|encode(T)];
encode([]) ->
    [].



group([]) ->
    [];
group([Item|Tail]) ->
    Name = Item#item.name,
    {This, Rest} = take_del(Name, Tail, [], []),
    S = [ Item | This ],
    [S | group(Rest)].

take_del(Name, [H|T], Take, Rest) ->
    if H#item.name == Name ->
            take_del(Name, T, [H|Take], Rest);
       true ->
            take_del(Name, T, Take, [H|Rest])
    end;
take_del(_, [], Take, Rest) ->
    {Take, Rest}.

choose(L) ->
    ?liof("Choosing from ~p~n", [length(L)]),
    Groups = group(L),
    ?liof("Choosing from ~p grouos ~n", [length(Groups)]),
    lists:flatten(choose2(Groups)).

choose2([G|Gs]) ->
    Item = hd(G),
    C = pick(G, Item#item.name),
    [C | choose2(Gs)];
choose2([]) ->
    [].

pick(L, _Name) ->
    Rated = lists:keysort(#item.rating, L),
    LongSort = lists:sort(
                 fun(X, Y) ->
                         (X#item.rating < Y#item.rating)
                             or
                             (X#item.time  > Y#item.time)
                 end, Rated),

    Songs = pick_songs(LongSort, 0),


    Calls = pick_calls(LongSort -- Songs, 0),

    Return = lists:append(Songs, Calls),

    Len = length(Return),
    if Len < 4 ->
            Xtr =  take_num(LongSort -- Return, 4-Len),
            Return ++ Xtr;
       true ->
            Return
    end.


take_num(_L, 0) ->
    [];
take_num([], _) ->
    [];
take_num([H|T], I) ->
    [H |take_num(T, I-1)].


pick_songs(_, 2) ->
    [];
pick_songs([], _) ->
    [];
pick_songs([H|T], I) ->
    case is_song(H) of
        true ->
            [H | pick_songs(T, I+1)];
        false ->
            pick_songs(T, I)
    end.


pick_calls(_, 2) ->
    [];
pick_calls([], _) ->
    [];
pick_calls([H|T], I) ->
    case is_song(H) of
        true ->
            [H | pick_calls(T, I+1)];
        false ->
            pick_calls(T, I)
    end.


is_song(Item) ->
    Sound = binary_to_list(Item#item.sound),
    lists:member(true,
                 [contains("song", Sound), contains("Song", Sound)]).

is_call(Item) ->
    Sound = binary_to_list(Item#item.sound),
    lists:member(true,
                      [contains("call", Sound), contains("Call", Sound)]).





contains(Small, Big)->
    string:str(Big, Small) > 0.


collect({_Div, _Attr, Stmts}) ->
    [Td1, Td2, Len, _Td4, _Date, _Time, Country, _Loc, _, Call , _, Rating| _]
        = Stmts,

    {_Div1, _, [Div2|_]} = Td1,
    {_, _, [Div3|_]} = Div2,
    {_, Attrs, _} = Div3,
    {value, {_,FilePath}} = lists:keysearch(<<"data-xc-filepath">>, 1, Attrs),

    {_, _, [Span|_]} = Td2,
    {_, _, [A|_]} = Span,
    {_,_, [Name|_]} = A,

    {_, _, [CallStr|_]} = Call,

    {_, _, XX} = Rating,
    [_,_, Div4 |_] = XX,
    {_, _, [UL|_]} = Div4,
    {_, _, Items} = UL,

    R = rating(Items),

    {_, _, [LenStr|_]} = Len,

    {_,_, [Ctr]} = Country,
    #item{name = Name,
          file = FilePath,
          sound = strip(CallStr),
          rating = R,
          time = strip(LenStr),
          country = Ctr}.


replace(_From, _To, []) ->
    [];
replace(From, To, [From |T]) ->
    [To | replace(From, To, T)];
replace(From , To, [H|T]) ->
    [H | replace(From, To, T)].

strip(S) ->
    R=lists:zf(fun(Char) ->
                      if Char == 32 ->
                              false;
                         Char == $\n ->
                              false;
                         true ->
                              {true, Char}
                      end
             end, binary_to_list(S)),
    list_to_binary(R).


rating([{_, Attr, [{_Span, _, [Rating|_]} |_]} | More]) ->
    case lists:keysearch(<<"class">>, 1, Attr) of
        {value, {_, <<"selected">>}} ->
            Rating;
        _ ->
            rating(More)
    end;
rating([]) ->
    <<"C">>.


p(L) ->
    lists:foreach(
      fun(X) ->
              ?liof("~p~n", [element(1, X)])
      end, L).

pr_stmt({Tag, _Attr, Ss}) ->
    ?liof("Tag: ~p -> ~p~n",[Tag, length(Ss)]).


f(F,A) ->
    io_lib:format(F,A).

