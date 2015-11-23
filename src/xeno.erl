-module(xeno).
-compile(export_all).

-include("yaws_api.hrl").

-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(cerror_msg(Fmt, Args),
        error_logger:error_msg("~w:~w: " ++ Fmt ++ "~p\n",
                               [?MODULE, ?LINE | Args] ++
                                   [erlang:get_stacktrace()])).

call_out(A) ->
     {ehtml,
      [{h1,[],"Xeno Canto Country Downloader"},
       {p, [],
        "This page can be used to grab and create MP3 recording "
        "collections from xeno-canto on per country basis."
        "It's most useful when you're going to travel to a country, and want"
        " a collection of recordings from that country (and neighboring "
        "countries as well."},

       {p, [],
        "Below is a table of previously generated country collections, use "
        "one of those instead of generatiing a new one"},

       {p, [],
        "The main feature here though, is the ability to - on the fly - "
        "generate new country collections, especially collections that "
        "consist of collections from several countries. "
        "For example a complete Europe list, could consist of a collection "
        "produced from a number of suitable European countries, or a good "
        "South-East Asia list could consist of a combined list from e.g "
        "Cambodia, Laos, Malaysia, Vietnam, Thailand"},

       {p,[],
        "When multiple countries are chosen, all data from all of them are"
        " grabbed, and then the selection algorithm is as follows:"},

       {ul, [],
        [
         {li, [],
          {p,[],
           "On a per species basis (all choosen countries), max 4 recordings"
           " are picked"
          }
         },

         {li, [],
          {p,[],
           "I favour high ratings, and long recording time"
          }
         },

         {li, [],
          {p,[],
           "Two 'call' and two 'song' recordings are picked (if available).
            Ff not the next (up to four) next best recordings
            are picked"
          }
         }]},

       {h2, [], "Generate a new collection"},

       {p, [],
        "Generating a collection of MP3s from Xen-canto typically takes "
        " some time, some countries have a massive amount of species, "
        " also please don't overuse this, i.e don't put too high load "
        " on the xeno-canto webservers. If you find a collection to your "
        " liking below, pick that instead of generating a new"
       },

       {p, [], "Enter a list of countries, use comma ',' to separate "
        "the countries. For example to generate a list for the nordic "
        " countries, enter 'Sweden, Norway, Denmark, Finland'"},

       {p, [], "It's important to spell the countries exactly right"},
       {p, [], "Just generating the list takes some considerable time, "
        "especially for countries with a large number of recordings"},

       {form, [{method, get},
               {action, "call_gen.yaws"}],
        [
         {p,[], "Enter comma separated list of countries"},
         {input, [{name, countries},
                   {type, text},
                   {size, 100}]}
        ]},
       {h2,[], "Previous lists"},
       front_gen_list(A)

      ]
     }.


front_gen_list(A) ->
    Dir = A#arg.docroot ++ "/calls/",
    file:make_dir(Dir),
    {ok, Collections} = file:list_dir(Dir),
    Map = lists:zf(
            fun(Coll) ->
                    %% e.g Syria

                    Base = filename:basename(Coll),
                    MetaFile = Dir ++ Coll ++ "/meta.bin",

                    try

                        {ok, B} = file:read_file(MetaFile),
                        Meta = binary_to_term(B),

                        {value, {countries, Countries}} =
                            lists:keysearch(countries, 1, Meta),
                        {value, {date, Date}} =
                            lists:keysearch(date, 1, Meta),
                        {value, {length, Len}} =
                            lists:keysearch(length, 1, Meta),
                        {value, {name, Name}} =
                            lists:keysearch(name, 1, Meta),

                        {true, {Coll, Base, Countries, Date, Len, Name}}
                    catch _:_ ->
                            false
                    end
            end, Collections),

    Row1 = {tr,[],
            [{td,[],{p,[],"Countries"}},
             {td,[],{p,[],"Date"}},
             {td,[],{p,[],"Num files"}},
             {td,[],{p,[],"Requestor"}},
             {td,[],{p,[],"URL"}}]},
    Rows = [ Row1 |
             lists:map(
               fun({Coll, Base, Countries, Date, Len, Name}) ->
                       {tr, [],
                        [
                         {td, [], fmt(Countries)},
                         {td, [], fmt_date(Date)},
                         {td, [], fmt(Len)},
                         {td, [], fmt(Name)},
                         {td, [], mk_url(A, Base)}
                        ]}
               end, Map)],

    {table,[{border, "1"}, {bordercolor, "black"}],
     Rows}.


mk_url(A, Dir) ->
    {a, [{href, "calls/" ++ Dir ++ "/zip" }],  Dir}.


fmt(T) ->
    io_lib:format("~p", [T]).

fmt_date({Y,M,D}) ->
    io_lib:format("~p-~p-~p", [Y,M,D]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-record(item, {
          name,
          file,
          sound,
          rating,
          time,
          country}).


t(Sp) ->
    {ok, B} = file:read_file("/Users/klacke/x"),
    All = binary_to_term(B),
    Choosen = choose(All),
    L = lists:foreach(
          fun(I = #item{name = N}) when N == Sp ->
                  pritem(I);
             (_) ->
                  ok
          end, Choosen).

pritems(Head, L) ->
    io:format("~s~n", [Head]),
    lists:foreach(
      fun(I = #item{name = N})  ->
              pritem(I);
         (_) ->
              ok
      end, L).

pritem(#item{name= Name, sound = Sound, rating = R, country = C, time=T}) ->
    io:format("~s ~s ~s ~s ~s~n", [Name, Sound, R, C, T]).

call_gen_out(A) ->
    P = yaws_api:parse_query(A),

    Countries0 = case lists:keysearch("countries", 1, P) of
                    {value, {_, Clist}} ->
                        CL = string:tokens(Clist, ", "),
                        Ret =lists:map(fun(C) -> string:strip(C, both) end, CL),
                        Ret;
              _ ->
                        Clist = "",
                        []
          end,
    Countries = lists:sort(Countries0),
    ibrowse:start(),
    All = lists:flatten(wget2(Countries)),
    file:write_file("/Users/klacke/x", term_to_binary(All)),
    Choosen = choose(All),

    prepare(Countries, Choosen),

    {ehtml,
     [{h1,[],"Xeno Canto Country Downloader"},
      {form,
       [{name, form},
        {method, post},
        {action, f("call_download.yaws?countries=~s", [Clist])}],

       [
        {p, [],
         io_lib:format(
           "A request can be sent to download an MP3 collection "
           " for ~p", [Countries])},

        total(Choosen),

        {p, [],
         "Please take care to not overuse this service, each of "
         "the download requests generated puts some considerable "
         " load on the Xeno Canto webservers"},

        {p, [],
         "Do you wish to make the request do download the list below? "
         "Once finished, your resulting collection will occur at "},

        {a, [{href, "call.yaws"}], "Xeno Canto country collector"},


        {p, [], ["Your name:",
                 {input, [{type, text}, {name, "dname"}]}]},

        {input, [{type, submit}, {value, "Download list"}]},


        {table, [{border, "black"}],
         [ row1() | make_rows(Choosen)]
        }
       ]}]}.


total(Chosen) ->
    Names = lists:map(fun(#item{name = N}) -> N end, Chosen),
    Uniq = uniq(Names),
    Len = length(Chosen),
    {p, [],
     io_lib:format("~p number of species in total ~p MP3 files",
                   [length(Uniq), Len])}.


uniq([H|T]) ->
    [ H | uniq(del_all(H, T))];
uniq([]) ->
    [].

del_all(H, [H|T]) ->
    del_all(H, T);
del_all(H, [X|T]) ->
    [ X | del_all(H, T)];
del_all(_,[])->
    [].


row1() ->
    {tr,[],
     [{td, [], {p, [], "Species"}},
      {td, [], {p, [], "Type"}},
      {td, [], {p, [], "Rating"}},
      {td, [], {p, [], "Length"}},
      {td, [], {p, [], "Country"}}]}.


make_rows(Ch) ->
    lists:map(
      fun(I) ->
              {tr,[],
               [{td, [], {p, [], I#item.name}},
                {td, [], {p, [], I#item.sound}},
                {td, [], {p, [], I#item.rating}},
                {td, [], {p, [], I#item.time}},
                {td, [], {p, [], I#item.country}}]}
      end, Ch).


gen_list(A) ->
    Dir = A#arg.docroot ++ "/calls/",
    {ok, Collections} = file:list_dir(Dir),
    Map = lists:map(
            fun(Coll) ->
                    %% e.g Syria
                    Base = filename:basename(Coll),
                    MetaFile = Dir ++ Coll ++ "/meta.bin",
                    {ok, B} = file:read_file(MetaFile),
                    Meta = binary_to_term(B),

                    {value, {countries, Countries}} =
                        lists:keysearch(countries, 1, Meta),
                    {value, {date, Date}} =
                        lists:keysearch(date, 1, Meta),
                    {value, {length, Len}} =
                        lists:keysearch(length, 1, Meta),

                    {Coll, Base, Countries, Date, Len}
            end, Collections),

    Row1 = {tr,[],
            [{td,[],{p,[],"Countries"}},
             {td,[],{p,[],"Date"}},
             {td,[],{p,[],"Num"}},
             {td,[],{p,[],"URL"}}]},
    Rows = [ Row1 |
             lists:map(
               fun({Coll, Base, Countries, Date, Len}) ->
                       {tr, [],
                        [
                         {td, [], Countries},
                         {td, [], fmt(Date)},
                         {td, [], fmt(Len)},
                         {td, [], mk_url(A, Base)}
                        ]}
               end, Map)],
    {table,[{border, "1"}, {bordercolor, "black"}],
     Rows}.




wget2([C|Countries]) ->
    ?liof("Getting Country ~p~n", [C]),
    CountryList = lists:flatten(wget3(C)),
    io:format("Got ~p for ~p~n", [length(CountryList), C]),
    Rest = wget2(Countries),
    CountryList ++ Rest;
wget2([]) ->
    [].

wget3(Country) ->
    wget4(string:strip(Country,both), 1).

wget4(Ctr,  I) ->
    Url = io_lib:format(
            "http://www.xeno-canto.org/explore?query=cnt%3A~s&pg=~w",[Ctr, I]),
    io:format("Url ~s~n", [Url]),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Attrs, Body} ->
            case body(Body) of
                done ->
                    [];
                List ->
                    [List , wget4(Ctr, I+1)]
            end;
        _Err ->
            io:format("URL ~p -> ~p~n",[Url, _Err]),
            []
    end.

body(Chars) ->
    P = mochiweb_html:parse(Chars),
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
    if length(Trs) == 0 ->
            done;
       true ->
            lists:zf(fun(Tr) ->
                             case catch collect(Tr) of
                                 {'EXIT', Err} ->
                                     ?cerror_msg("~p~n", [Err]),
                                     io:format("DROP \n~p~n",[Tr]),
                                     false;
                                 Ret ->
                                     {true, Ret}
                             end
                     end,Trs)
    end.


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
    Groups = group(L),
    lists:flatten(choose2(Groups)).

choose2([G|Gs]) ->
    Item = hd(G),
    C = pick(G, Item#item.name),
    [C | choose2(Gs)];
choose2([]) ->
    [].

pick(L, Name) ->
    Rated = lists:keysort(#item.rating, L),

    LongSort = lists:sort(
                 fun(X, Y) ->
                         (X#item.rating < Y#item.rating)
                             or
                             (X#item.time  > Y#item.time)
                 end, Rated),


    if Name == <<"Yellowhammer">> ->
            pritems("** Longsort", LongSort);
       true ->
            ok
    end,

    Songs = pick_songs(LongSort, 0),

    if Name == <<"Yellowhammer">> ->
            pritems("** Songs", Songs);
       true ->
            ok
    end,


    Calls = pick_calls(LongSort, 0),

    Return = lists:append(Songs, Calls),

    if Name == <<"Yellowhammer">> ->
            io:format("XXXX Cals ~p~nSongs:~p~nRet~p~n",
                      [Calls, Songs, Return]);
            %pritems("** Calls", Calls);
       true ->
            ok
    end,


    if Name == <<"Yellowhammer">> ->
            io:format("** Ret ~p~n", [length(Return)]);
       true ->
            ok
    end,

    Len = length(Return),
    if Len < 4 ->
            Xtr =  take_num(LongSort -- Return, 4-Len),
            Return ++ Xtr;
       true ->
            Return
    end.


take_num(L, 0) ->
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
    case lists:member(true,
                      [contains("song", Sound), contains("Song", Sound)]) of
        true ->
            not is_call(Item);
        false ->
            false
    end.


is_call(Item) ->
    Sound = binary_to_list(Item#item.sound),
    case lists:member(true,
                      [contains("call", Sound), contains("Call", Sound)]) of
        true ->
            not is_song(Item);
        false ->
            false
    end.




contains(Sub, Str) ->
    contains(Sub, Sub, Str).

contains(Sub, [H1|T1], [H1 |T2]) ->
    contains(Sub, T1, T2);
contains(_, [], _) ->
    true;
contains(_,_,[]) ->
    false;
contains(Sub, [H1|T1], [H2|T2]) ->
    %% restart
    contains(Sub, Sub, T2).


%% test
aa() ->
    {ok, Bin} = file:read_file("laos.html"),
    P = mochiweb_html:parse(Bin),
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

    lists:map(
      fun(Tr) ->
              case catch collect(Tr) of
                  {'EXIT', Err} ->
                      io:format("~p~n~p~n", [Err, erlang:get_stacktrace()]),
                      io:format("~p~n", [Tr]);
                  _ ->
                      ok
              end
      end,Trs).

collect({_Div, _Attr, Stmts}) ->
    [Td1, Td2, Len, Td4, _Date, Time, Country, Loc, _, Call , _, Rating| _]
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
              io:format("~p~n", [element(1, X)])
      end, L).

pr_stmt({Tag, Attr, Ss}) ->
    io:format("Tag: ~p -> ~p~n",[Tag, length(Ss)]).


f(F,A) ->
    io_lib:format(F,A).

prepare(Countries, Choosen) ->
    case whereis(gen_call) of
        undefined ->
            S = self(),
            register(gen_call, proc_lib:spawn(
                                 fun() -> S ! done,
                                          gen_caller()
                                 end)),
            receive
                done ->
                    ok
            end;
        _ ->
            ok
    end,

    gen_call ! {self(), {prepare, Countries, Choosen}},
    receive
      {gen_call, Res} ->
            Res
    end.

generate(Countries, Name, Docroot) ->
    Dir = Docroot ++ "/calls/" ++ dir_str(Countries),
    case file:read_file_info(Dir) of
        {ok, _} ->
            {exists, Dir};
        _ ->
            gen_call ! {self(), {generate, Countries, Name, Docroot}},
            receive
                {gen_call, Res} ->
                    Res
            end
    end.

gen_caller() ->
    process_flag(trap_exit, true),
    gen_caller([]).
gen_caller(Lists) ->
    receive
        {From, {prepare,  Countries, Choosen}} ->
            From ! {gen_call, ok},
            gen_caller([{false, Countries, Choosen, noname} | Lists]);
        {From, {generate, Countries, Name, Docroot}} ->
            case lists:keysearch(Countries, 2, Lists) of
                {value, {false, _,  Choosen, _}} ->
                    ?liof("spawning \n",[]),
                    Pid = proc_lib:spawn_link(
                            fun() ->
                                    do_generate(Countries, Choosen,
                                                Name, Docroot)
                            end),
                    L2 = lists:keydelete(Countries, 2, Lists),
                    From ! {gen_call, ok},
                    gen_caller([{Pid, Countries, Choosen, Name} | L2]);
                {value, {Pid, Countries, _Choosen2, Name2}} ->
                    ?liof("running \n",[]),
                    From  ! {gen_call, {running, Name2}},
                    gen_caller(Lists);
                false ->
                    ?liof("none \n"
                          "Lists = ~p~n"
                          "Countries = ~p~n", [Lists, Countries]),
                    From  ! {gen_call, none},
                    gen_caller(Lists)
            end;
        {From, ongoing} ->
            From ! {gen_call,
                    lists:zf(
                      fun({Pid, Countries, _Choosen2, Name2}) ->
                              if is_pid(Pid) ->
                                      Pid ! {self(), get_num},
                                      receive
                                          {Pid, {Num, Tot}} ->
                                              {true, {Name2, Countries,
                                                      Num, Tot}};
                                          {EXIT, Pid, _} ->
                                              false
                                      end;
                                 true ->
                                      false
                              end
                      end, Lists)},
            gen_caller(Lists);
        {'EXIT', From, _} ->
            gen_caller(lists:keydelete(1, From, Lists))
    end.

dir_str([Last]) ->
    Last;
dir_str([H|T]) ->
    H ++ "-" ++ dir_str(T).

do_generate(Countries, Choosen, Name, Docroot) ->
    try
        do_generate2(Countries, Choosen, Name, Docroot)
    catch X:Y ->
            ?cerror_msg("~p", [{X,Y}])
    end.

do_generate2(Countries, Choosen, Name, Docroot) ->
    file:make_dir(Docroot ++ "/calls/"),
    Dir = Docroot ++ "/calls/" ++ dir_str(Countries),
    ok = file:make_dir(Dir),
    Meta = [{countries, Countries},
            {date, date()},
            {name, Name},
            {length, length(Choosen)}],

    file:make_dir(Dir ++ "/zip"),
    ?liof("here \n",[]),
    Choosen2 = lists:zip(Choosen, lists:seq(1, length(Choosen))),
    lists:foreach(
      fun({Item, Number}) ->
              receive
                  {From, get_num} ->
                      From ! {Number, length(Choosen)}
              after 0 ->
                      ok
              end,
              Mp3 = "'" ++ Dir ++
                  "/zip/" ++
                  dash_name(binary_to_list(Item#item.name)) ++
                  "[" ++
                  dash_name(binary_to_list(Item#item.country)) ++
                  "(" ++ integer_to_list(Number) ++ ")" ++
                  "].mp3'",
              io:format("M: ~p~n", [Mp3]),
                      io:format("Downloading ~p~nto~p~n",
                                [Item#item.file, Mp3]),
              Cmd = lists:flatten(
                      ["curl ",
                      binary_to_list(Item#item.file), " > ", Mp3]),
              ?liof("CMD: ~s", [Cmd]),
              os:cmd(Cmd),
              io:format("DONE \n",[])
              %%os:cmd(["touch ", Mp3])
      end, Choosen2),
    file:write_file(Dir ++ "/meta.bin", term_to_binary(Meta)),
    ?liof("Done downloadeing ~p~n", [Meta]).


dash_name(L) ->
    lists:map(fun(Char) ->
                      if Char == 32 ->
                              $-;
                         true ->
                              Char
                      end
              end, L).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


call_download_out(A) ->
    P = yaws_api:parse_query(A),
    CliD = binary_to_list(A#arg.clidata),

    Countries = case lists:keysearch("countries", 1, P) of
              {value, {_, Clist}} ->
                  string:tokens(Clist, ", ");
              _ ->
                  []
          end,

    case CliD of
        [] ->
            {ehtml,
             [{h1,[],"Xeno Canto Country Downloader"},
              {p, [], "A name of the downloader is required, try again"}]};
        Str ->
            ["dname", Name] = string:tokens(Str, "="),

            case generate(Countries, Name, A#arg.docroot) of
                ok ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      hours(),
                      {p, [], ["Download in progress ..."
                               "check later for your compiled lists with "
                               "your name at: ",
                               {a, [{href, "call.yaws"}],
                                    "Xeno Cantor downloader"}]}]};
                {running, Name2} ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("A download for ~p is already in progress"
                                "by ~s",
                                [Countries, Name2])}]};
                {exists, Dir} ->
                    os:cmd(["rm -rf " , Dir]),
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("A compilation for ~p already existed, "
                                "It's now removed and a new download is "
                                " in progress ",
                                [Countries])},
                      hours(),
                      {p, [], ["Download in progress ..."
                               "check later for your compiled lists with "
                               "your name at: ",
                               {a, [{href, "call.yaws"}],
                                    "Xeno Cantor downloader"}]}]};
                none ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("Nothing found - retry please",
                                [])}]};
                _X ->
                    error_logger:format("XXX ~p ~n", [_X]),
                    ok
            end
    end.


hours() ->
    {p,[],
     "Large collections can take hours to download, don't despair "
     "and don't immdiately retry if it's not yet there"}.

