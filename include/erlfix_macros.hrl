%% Author: Nikolay Volnov

%%-ifdef(debug).
%%    -define(DBG(Str,Args), io:format(Str ++ "~n",Args)).
%%      -define(DGB(Str,Args), 
%%			  error_logger:info_msg("~p (line ~p): " ++ Str ++ "~n"),
%%		[?MODULE, ?LINE | Args]).

-define(DBG(Str,Args), error_logger:info_msg("~w (line ~w): " ++ Str ++ "~n", [?MODULE, ?LINE | Args])).
%%-define(DBG(Str,Args), erlang:display(Str)).

%%-else.
%%    -define(DBG(Str,Args), void).
%%-endif.