-module(xeno_pick).
-compile(export_all).

-include("yaws_api.hrl").


-define(liof(Fmt, Args),
        io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
%-define(liof(Fmt, Args), ok).

-define(cerror_msg(Fmt, Args),
        error_logger:error_msg("~w:~w: " ++ Fmt ++ "~p\n",
                               [?MODULE, ?LINE | Args] ++
                                   [erlang:get_stacktrace()])).





%% From top, index.yaws


call_out(A) ->

    {ok, Bin} = file:read_file(A#arg.docroot ++ "/xeno/DATA/checklists.bin"),
    CL = lists:keysort(1, binary_to_term(Bin)),

     {ehtml,
      [{h1,[],"Xeno Canto Country Downloader"},
       {p, [],
        "This page can be used to grab and create MP3 recording "
        "collections from xeno-canto on per country basis."
        "It's most useful when you're going to travel to a country, and want"
        " a collection of recordings from that country (and neighboring "
        "countries as well. "},
       {p, [], 
        "The collection is based on the aggregated checklist (IOC) from the"
        " choosen countries, then the recordings are picked from xeno-canto"
        " irrespective of which counry the recording is made in"},
        

       {p, [], ["The code for this is open source and resides on github at ",
                {a, [{href, "https://github.com/klacke/xeno-canto-country-collector"}],
                 "https://github.com/klacke/xeno-canto-country-collector"}]},

       {p, [],
        "For example a complete WP list, could consist of a collection "
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


       {form, [%{method, post},
               {action, "xeno_gen.yaws"}],
        [
         {table, [],
          lists:map(fun({Country, _}) ->
                            {tr, [],
                             [{td, [],
                               [{input, [{name, Country},{value, Country},
                                         {type, checkbox}
                                        ], Country}]}]}
                    end, CL)
          },
         {input, [{value, "Generate ZIP file"}, {name, "ZIP"},
                  {type, submit}, "ZIP"
                 ]},

         {input, [{value, "Show computed Checklist"}, {name, "Checklist"},
                  {type, submit}, "Checklist"
                 ]}
        ]
       },

       {p, [], "Author: Claes Wikstrom"}
      ]
     }.



%% Once submit is hit on first page

call_gen_out(A) ->
    CLFILE = A#arg.docroot ++ "/xeno/DATA/checklists.bin",
    {ok, Bin} = file:read_file(CLFILE),
    CL = lists:keysort(1, binary_to_term(Bin)),


    P = yaws_api:parse_query(A),
    P2 = [ X || { X,_Y} <- P ],
    %io:format("P2 = ~p~n", [P2]),
    Countries = P2 -- ["Checklist", "ZIP"],

    T = ets:new(list, [set, public]),
    lists:foreach(
      fun(Country) ->
              case lists:keysearch(Country, 1, CL) of
                  {value, {_, Birds}} ->
                      lists:foreach(fun(X) ->
                                            ets:insert(T, X)
                                    end, Birds);
                  _ ->
                      ok
              end
      end, P2),


    case {lists:member("Checklist", P2), lists:member("ZIP", P2)}  of
        {true, _} ->

            %% Calculate num species

            EHTML = 
            {ehtml,
             [{h1,[],"Checklist ......"},
              {h2, [], "Countries: " ++ [C ++ ", " || C <- Countries]},

              {table, [],
               [{tr, [],
                 [{td, [], "Name"},
                  {td, [], "Scientific name"},
                  {td, [], "Number of recordings"}
                 ]
                }] ++
                   lists:filtermap(
                     fun({Name, Latin}) ->
                             Dir = A#arg.docroot ++
                                 "/xeno/DATA/calls/" ++ binary_to_list(Name),
                             case file:list_dir(Dir) of
                                 {ok, Files} ->
                                     Num = length(Files),
                                     TR = {tr, [],
                                           [{td, [], Name},
                                            {td, [], {i, [], Latin}},
                                            {td, [], integer_to_list(Num)}
                                           ]
                                          },
                                     {true, TR};
                                 _ ->
                                     io:format("NO dir ~p~n", [Dir]),
                                     false
                             end
                     end, lists:sort(ets:tab2list(T)))
              }]},
            ets:delete(T),
            EHTML;
        {_, true} ->

            DocRoot = A#arg.docroot,
            CName = string:join(Countries, "_"),

            RelZipDir = "xeno/zip/" ++ CName,
            AbsZipDir = DocRoot ++ "/" ++ RelZipDir,

            os:cmd("mkdir -p " ++ "'" ++ AbsZipDir ++ "'"),
            os:cmd("rm -f " ++ "'" ++ AbsZipDir ++ "'" ++ ".zip"),
            clean("'" ++ AbsZipDir ++ "'"),
            clean("'" ++ AbsZipDir ++ "'" ++ ".zip"),

            lists:foreach(
              fun({Name, _Latin}) ->
                      Dir = A#arg.docroot ++ "/xeno/DATA/calls/" ++
                          binary_to_list(Name),
                      case file:list_dir(Dir) of
                          {ok, Files} ->
                              lists:foreach(
                                fun(File) ->
                                        Src = Dir ++ "/" ++ File,
                                        Dest = AbsZipDir ++ "/" ++ File,
                                        file:make_symlink(Src, Dest)
                                end, Files);
                          _ ->
                              ok
                      end
              end, ets:tab2list(T)),

            spawn(fun() ->
                          Q = "'" ++ CName ++"'",
                          Zip = DocRoot ++ "/xeno/zip",
                          Cmd = "zip -q -r " ++ Q ++ " "
                                            ++ Q,
                          Port = open_port({spawn, Cmd},
                                           [stderr_to_stdout,
                                            {cd, Zip}, stream, eof]),
                          rec_loop(Port)
                  end),
            
                          
            ets:delete(T),


            {ehtml,
             [{h1,[],"Xeno Canto Country Downloader ......"},
              {p, [], "ZIP file... for:"},
              {p, [], "Countries: " ++ [C ++ ", " || C <- Countries]},
              {p, [], ["The full ZIP file will occur at the link below" ++
                           " within 10 minutes"]},
              {a, [{href, "zip/" ++ CName ++ ".zip"}],
               "ZIP file for " ++ CName}
              
              
             ]
            }


    end.

rec_loop(P) ->
    receive
        {P, {data, BinData}} ->
            io:format("GOT ~p~n",[BinData]),
            rec_loop(P);
        {P, eof} ->
            ok
    end.

clean(Dir) ->
    spawn(fun() ->
                  receive
                  after (1000 * 3600 * 28) ->
                          os:cmd("rm -rf " ++ Dir)
                  end
          end).

                    


