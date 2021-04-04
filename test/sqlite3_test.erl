-module(sqlite3_test).
%% To run it use
%% rebar3 eunit  -m sqlite3_test



-include_lib("eunit/include/eunit.hrl").

-define(FuncTest(Name), {??Name, fun Name/0}).


%% Generator function to make a loop to run tests one by one
%% with open and close
all_test_() ->
    { setup,
      fun open_db/0,
      fun close_db/1,
     [?FuncTest(basic_functionality)]}.

drop_all_tables(Db) ->
    Tables = sqlite3:list_tables(Db),
    [sqlite3:drop_table(Db, Table) || Table <- Tables],
    Tables.

open_db() ->
    %sqlite3:open(ct, [in_memory]).
    sqlite3:open(ct, [{file, "./history_test.db"}]),
    drop_all_tables(ct).

close_db({ok, _Pid}) ->
    error_logger:info_msg("Closing ~p~n", [ _Pid ]),
    sqlite3:close(ct);
close_db(_) ->
    ok.

basic_functionality() ->
    TableInfo = [{id, integer, [{primary_key, [asc, autoincrement]}]},
                {name, text, [not_null, unique]},
                {age, integer, not_null},
                {wage, integer}],
    ok = sqlite3:create_table(ct, user, TableInfo),
    ?assertEqual(
        [sqlite_sequence,user],
        sqlite3:list_tables(ct)).

