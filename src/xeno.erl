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



call_out(A) ->
     {ehtml,
      [{h1,[],"Xeno Canto Country Downloader"},
       {p, [],
        "This page can be used to grab and create MP3 recording "
        "collections from xeno-canto on per country basis."
        "It's most useful when you're going to travel to a country, and want"
        " a collection of recordings from that country (and neighboring "
        "countries as well."},

       {p, [], ["The code for this is open source and resides on github at ",
                {a, [{href, "https://github.com/klacke/xeno-canto-country-collector"}],
                 "https://github.com/klacke/xeno-canto-country-collector"}]},
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
            If not, the next (up to four) next best recordings
            are picked."
          }
         }]},


       {h2, [], "Generate a new collection"},

       {p, [],
        "Generating a collection of MP3s from Xeno-canto typically takes "
        " some time, some countries have a massive amount of species, "
        " also please don't overuse this, i.e don't put too high load "
        " on the xeno-canto web servers. If you find a collection to your "
        " liking below, pick that instead of generating a new."
       },

       {p, [], "Enter a list of countries, use comma ',' to separate "
        "the countries. For example to generate a list for the nordic "
        " countries, enter 'Sweden, Norway, Denmark, Finland'"},

       {p, [], "It's important to spell the countries exactly right."},
       {p, [], "Just generating the list takes some considerable time, "
        "especially for countries with a large number of recordings, "
        "there are no"
        " spinning wheels or anything, just be patient"},

       {form, [{method, get},
               {action, "xeno_gen.yaws"}],
        [
         {p,[], "Enter 'comma' separated list of countries"},
         {input, [{name, countries},
                   {type, text},
                   {size, 100}]}
        ]},
       {h2,[], "Previous lists"},
       front_gen_list(A),
       {p, [], "Author: Claes Wikstrom"}
      ]
     }.


front_gen_list(A) ->
    Dir = A#arg.docroot ++ "/xeno/calls/",
    file:make_dir(Dir),
    {ok, Collections} = file:list_dir(Dir),
    Map = lists:zf(
            fun(Coll) ->
                    %% e.g Syria
                    try
                        Base = filename:basename(Coll),
                        MetaFile = Dir ++ Coll ++ "/meta.bin",
                        {ok, B} = file:read_file(MetaFile),
                        Meta = binary_to_term(B),
                        ?liof("Meta = ~p~n", [Meta]),

                        {value, {_, Countries}} =
                            lists:keysearch(countries, 1, Meta),
                        {value, {_, Date}} =
                            lists:keysearch(date, 1, Meta),
                        {value, {_, Len}} =
                            lists:keysearch(length, 1, Meta),
                        {value, {_, Name}} =
                            lists:keysearch(name, 1, Meta),
                        {value, {_, Done}} =
                            lists:keysearch(done, 1, Meta),
                        {value, {_, Num}} =
                            lists:keysearch(numspecs, 1, Meta),
                        {true, {Coll, Base, Countries, Date,
                                Len, Name, Done, Num}}
                    catch _:_ ->
                            ?cerror_msg("Drop meta for ~p~n", [Coll]),
                            false
                    end

            end, Collections),

    Row1 = {tr,[],
            [{td,[],{p,[],"Countries"}},
             {td,[],{p,[],"Date"}},
             {td,[],{p,[],"Num files"}},
             {td,[],{p,[],"Num species"}},
             {td,[],{p,[],"Requestor"}},
             {td,[],{p,[],"URL"}},
             {td,[],{p,[],"Size"}}
            ]},
    Rows = [ Row1 |
             lists:map(
               fun({Collection, Base, Countries, Date, Len,
                    Name, Done, Num}) ->
                       {tr, [],
                        [
                         {td, [], fmt_c(Countries)},
                         {td, [], fmt_date(Date)},
                         {td, [], fmt(Len)},
                         {td, [], fmt(Num)},
                         {td, [], fmt(Name)},
                         {td, [], mk_url(A, Base, Done, Collection)},
                         {td, [], mk_sz(A, Base, Done, Collection)}
                        ]}
               end, Map)],

    {table,[{border, "1"}, {bordercolor, "black"}],
     Rows}.

mk_url(_A, _Dir, false, _) ->
    {p, [], "Still downloading...."};
mk_url(_A, Dir, true, Cdir) ->
    {a, [{href, "/xeno/calls/" ++ Dir ++ "/" ++ Cdir ++ ".tar.gz" }],  Cdir}.

mk_sz(_A, _Dir, false, _) ->
    {p,[], "N/A"};
mk_sz(A, Dir, true, Cdir) ->
    File = A#arg.docroot ++ "/xeno/calls/" ++ Dir ++ "/" ++ Cdir
        ++ ".tar.gz",
    ?liof("File ~s~n", [File]),
    {ok, FI} = file:read_file_info(File),
    Sz = element(2, FI),
    {p, [], human_filesize(Sz)}.

human_filesize(Size) ->
     human_filesize(Size, ["B","KB","MB","GB","TB","PB"]).

human_filesize(S, [_|[_|_] = L]) when S >= 1024 ->
     human_filesize(S/1024, L);
human_filesize(S, [M|_]) ->
    
    io_lib:format("~.2f ~s", [float(S), M]).

join(List, Sep) ->
    join(List, Sep, []).

join([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
join(X=[_], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc, X));
join([H|T], Sep, Acc) ->
    join(T, Sep, [Sep,H|Acc]).

fmt_c(Countries) ->
    {p, [], join(Countries, "<br/>")}.

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
    lists:foreach(
          fun(I = #item{name = N}) when N == Sp ->
                  pritem(I);
             (_) ->
                  ok
          end, Choosen).

pritems(Head, L) ->
    io:format("~s (~p)~n", [Head, length(L)]),
    lists:foreach(
      fun(I = #item{})  ->
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
                        CL = string:tokens(Clist, ","),
                        Ret =lists:map(fun(C) -> string:strip(C, both) end, CL),
                        Ret;
              _ ->
                        Clist = "",
                        []
          end,
    Countries = lists:sort(Countries0),
    ibrowse:start(),
    All = lists:flatten(wget2(Countries)),
    %file:write_file("/Users/klacke/x", term_to_binary(All)),
    Choosen = choose(All),

    prepare(Countries, Choosen),

    {ehtml,
     [{h1,[],"Xeno Canto Country Downloader"},
      {form,
       [{name, form},
        {method, post},
        {action, f("xeno_download.yaws?countries=~s", [Clist])}],

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

        {a, [{href, "index.yaws"}], "Xeno Canto country collector"},


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


wget2([C|Countries]) ->
    ?liof("Getting Country ~p~n", [C]),
    CountryList = lists:flatten(wget3(C)),
    ?liof("Got ~p for ~p~n", [length(CountryList), C]),
    Rest = wget2(Countries),
    CountryList ++ Rest;
wget2([]) ->
    [].

wget3(Country) ->
    wget4(string:strip(Country,both), 1).

wget4(Ctr,  I) ->
    Url = io_lib:format(
            "http://www.xeno-canto.org/explore?query=cnt%3A\"~s\"&pg=~w",
            [replace($\s, $+, Ctr), I]),
    ?liof("Url ~s~n", [Url]),
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Attrs, Body} ->
            case body(Body) of
                done ->
                    [];
                List ->
                    [List , wget4(Ctr, I+1)]
            end;
        _Err ->
            error_logger:error_msg("URL ~p -> ~p~n",[Url, _Err]),
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
                                 {'EXIT', _Err} ->
                                     %?cerror_msg("~p~n", [Err]),
                                     %?liof("DROP \n~p~n",[Tr]),
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
                      ?liof("~p~n~p~n", [Err, erlang:get_stacktrace()]),
                      ?liof("~p~n", [Tr]);
                  _ ->
                      ok
              end
      end,Trs).

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

generate(_Countries, "", _Docroot) ->
    noname;
generate(Countries, Name, Docroot) ->
    gen_call ! {self(), {generate, Countries, Name, Docroot}},
    receive
        {gen_call, Res} ->
            Res
    end.

gen_caller() ->
    process_flag(trap_exit, true),
    try gen_caller([], none)
    catch X:Y ->
            ?cerror_msg("~p", [{X,Y}])
    end.

gen_caller(Prep, Mode) ->
    receive
        {From, {prepare, Countries, Choosen}} ->
            From ! {gen_call, ok},
            gen_caller([{prepared, Countries, Choosen}| Prep], Mode);
        {From, {generate, Countries, Name, Docroot}} ->
            Dir  = Docroot ++ "/xeno/calls/" ++ dir_str(Countries),
            Prev = case file:read_file_info(Dir) of
                       {ok, _} ->
                           case file:read_file_info(Dir ++ "/meta.bin") of
                               {ok, _} ->
                                   {exists, Countries};
                               _ ->
                                   {running, Countries}
                           end;
                       _ ->
                           ok
                   end,
            case lists:keysearch(Countries, 2, Prep) of
                false ->
                    From  ! {gen_call, none},
                    gen_caller(Prep, Mode);
                {value, {prepared, _, Choosen}} ->
                    case Mode of
                        none ->
                            os:cmd(["rm -rf " , Dir]),
                            Pid = proc_lib:spawn_link(
                                    fun() ->
                                            do_generate(Countries, Choosen,
                                                        Name, Docroot)
                                    end),
                            %% 1 hours, cleanup
                            timer:send_after(1000 * 3600 * 1, self(),
                                             {clean,Dir,Pid}),
                            
                            From ! {gen_call, Prev},
                            Mode2 = {generating, Pid, Countries, Choosen,
                                     Name},
                            Prep2 = lists:keydelete(Countries, 2, Prep),
                            gen_caller(Prep2, Mode2);
                        {generating, _, Countries2, _, Name} ->
                            From ! {gen_call, {occupied, Countries2, Name}},
                            gen_caller(Prep, Mode)
                    end
            end;
        {clean, Dir, Pid} ->
            case Mode of
                {generating, Pid, _Countries2, _, _Name} ->
                    exit(Pid, kill),
                    os:cmd([" rm -rf ", Dir]),
                    gen_caller(Prep, none);
                _ ->
                    gen_caller(Prep, Mode)
            end;
        {'EXIT', Pid, _} ->
            case Mode of
                {generating, Pid, _Countries2, _, _Name} ->
                    gen_caller(Prep, none);
                _ ->
                    gen_caller(Prep, Mode)
            end
    end.

dir_str([Last]) ->
    replace($\s, $_, Last);
dir_str([H|T]) ->
    replace($\s, $_, H) ++ "-" ++ dir_str(T).

do_generate(Countries, Choosen, Name, Docroot) ->
    try
        do_generate2(Countries, Choosen, Name, Docroot)
    catch X:Y ->
            ?cerror_msg("~p", [{X,Y}])
    end.

do_generate2(Countries, Choosen, Name, Docroot) ->
    file:make_dir(Docroot ++ "/xeno/calls/"),
    CountryDir = dir_str(Countries),
    Dir = Docroot ++ "/xeno/calls/" ++ CountryDir, 
    file:make_dir(Dir),

    NumSpecs = length(
                 uniq(lists:map(fun(#item{name = N}) -> N end, Choosen))),

    Meta = [{countries, Countries},
            {date, date()},
            {name, Name},
            {done, false},
            {numspecs, NumSpecs},
            {length, length(Choosen)}],
    file:write_file(Dir ++ "/meta.bin", term_to_binary(Meta)),

    file:make_dir(Dir ++ "/" ++ CountryDir),
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
                  "/" ++ CountryDir ++ "/" ++
                  replace($\s, $-, binary_to_list(Item#item.name)) ++
                  "[" ++
                  replace($\s, $-, binary_to_list(Item#item.country)) ++
                  "(" ++ integer_to_list(Number) ++ ")" ++
                  "].mp3'",
              ?liof("M: ~p~n", [Mp3]),
              ?liof("Downloading ~p~nto~p~n",
                    [Item#item.file, Mp3]),
              Cmd = lists:flatten(
                      ["curl ",
                      binary_to_list(Item#item.file), " > ", Mp3]),
              ?liof("CMD: ~s", [Cmd]),
              os:cmd(Cmd),
              ?liof("DONE \n",[])
      end, Choosen2),
    Meta2 = [{countries, Countries},
             {date, date()},
             {name, Name},
             {done, true},
             {numspecs, NumSpecs},
             {length, length(Choosen)}],
    os:cmd(_Cmd = ["cd ", Dir, "; tar cfz ", CountryDir,
                        ".tar.gz ", CountryDir,
                        "; rm -rf ", CountryDir]), 
    file:write_file(Dir ++ "/meta.bin", term_to_binary(Meta2)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


call_download_out(A) ->
    P = yaws_api:parse_query(A),
    CliD = binary_to_list(A#arg.clidata),

    Countries = case lists:keysearch("countries", 1, P) of
                    {value, {_, Clist}} ->
                        Tks = string:tokens(Clist, ","),
                        lists:sort(lists:map(fun(Tk) -> string:strip(Tk, both) end, Tks));
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
                      dl()]};
                noname ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], "Must provide a name, retry !!"}]};

                {running, Countries} ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("A download for ~p is already in progress"
                                ", alternatively it has been aborted earlier ",
                                [Countries])},
                      {p, [], "It has been cancelled and replaced with your request"},
                      hours(), dl()]};

                {exists, Countries} ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("A compilation for ~p already existed, "
                                "It's now removed and a new download is "
                                " in progress ",
                                [Countries])},
                      hours(),
                      dl()]};
                {occupied, Countries2, Name} ->
                    {ehtml,
                     [{h1,[],"Xeno Canto Country Downloader"},
                      {p, [], f("A compilation for ~p is currently running "
                                "on behalf of ~s",
                                [Countries2, Name])},
                      {p, [], "I only allow one concurrent compilation at a "
                       "time due to memory constraints on the  host "
                       "where this service "
                       "is running."
                       " Please retry this country compilation later "}]};

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


dl() ->
    {p, [], ["Download in progress ..."
             "check later for your compiled lists with "
             "your name at: ",
             {a, [{href, "index.yaws"}],
              "Xeno Cantor downloader"}]}.


hours() ->
    {p,[],
     "Large collections can take hours to download, don't despair "
     "and don't immdiately retry if it's not yet there"}.

