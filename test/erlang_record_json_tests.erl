-module(erlang_record_json_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("erlang_record_json.hrl").

start() ->
    erlang_record_json_utils:init_record_fields().

stop(_) ->
    erlang_record_json_utils:terminate_record_fields().

%%--------------------------------------------------------------------
%% record to json tests
%%--------------------------------------------------------------------

to_json_test_() ->
    {foreach, fun start/0, fun stop/1, [fun to_json_flat_ok/1, fun to_json_nested_ok/1]}.
to_json_flat_ok(_) ->
    fun() ->
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    FlatJson = <<"{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\",\"fields\":null}">>,
	    Json = unicode:characters_to_binary(erlang_record_json:to_json(FlatRecord, #erlang_record_flat{}, erlang_record_json_utils)),
	    ?assertEqual(FlatJson, Json)
    end.
to_json_nested_ok(_) -> 
    fun() ->
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    NestedRecord = #erlang_record_nested{id="r2", name="record2", type=nested, children=[FlatRecord]},
	    NestedJson = <<"{\"id\":\"r2\",\"name\":\"record2\",\"type\":\"nested\",\"fields\":null,\"children\":[{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\",\"fields\":null}]}">>,
	    Json = unicode:characters_to_binary(erlang_record_json:to_json(NestedRecord, #erlang_record_nested{}, erlang_record_json_utils)),
	    ?assertEqual(NestedJson, Json)
    end.

to_sanitised_json_test_() ->
    {foreach, fun start/0, fun stop/1, [fun to_sanitised_json_flat_ok/1, fun to_sanitised_json_nested_ok/1]}.
to_sanitised_json_flat_ok(_) ->
    fun() ->
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    FlatJson = <<"{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}">>,
	    ?assertEqual(FlatJson, unicode:characters_to_binary(erlang_record_json:to_sanitised_json(FlatRecord, #erlang_record_flat{}, erlang_record_json_utils)))
    end.
to_sanitised_json_nested_ok(_) ->
    fun() ->
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    NestedRecord = #erlang_record_nested{id="r2", name="record2", type=nested, children=[FlatRecord]},
	    NestedJson = <<"{\"id\":\"r2\",\"name\":\"record2\",\"type\":\"nested\",\"children\":[{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}]}">>,
	    ?assertEqual(NestedJson, unicode:characters_to_binary(erlang_record_json:to_sanitised_json(NestedRecord, 
												       #erlang_record_nested{}, erlang_record_json_utils)))
    end.

to_record_test_() ->
    {foreach, fun start/0, fun stop/1, [fun to_record_flat_ok/1, 
					fun to_record_nested_ok/1, 
					fun to_record_samefield_flat_ok/1, 
					fun to_record_samefield_nested_ok/1]}.
to_record_flat_ok(_) ->
    fun() ->	    
	    FlatJson = <<"{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}">>,
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    ?assertEqual(FlatRecord, erlang_record_json:to_record(FlatJson, #erlang_record_flat{}, erlang_record_json_utils))
    end.
to_record_nested_ok(_) ->
    fun() ->
	    NestedJson = <<"{\"id\":\"r2\",\"name\":\"record2\",\"type\":\"nested\",\"children\":[{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}]}">>,
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    NestedRecord = #erlang_record_nested{id="r2", name="record2", type="nested", children=[FlatRecord]},
	    ?assertEqual(NestedRecord, erlang_record_json:to_record(NestedJson, #erlang_record_nested{}, erlang_record_json_utils))
    end.

to_record_samefield_flat_ok(_) ->
    fun() ->
	    FlatJson = <<"{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}">>,
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    ?assertEqual(FlatRecord, erlang_record_json:to_record(FlatJson, 
								  [#erlang_record_flat{type="flat"}, #erlang_record_nested{type="nested"}], 
								  type, 
								  erlang_record_json_utils))
    end.

to_record_samefield_nested_ok(_) ->
    fun() ->
	    NestedJson = <<"{\"id\":\"r2\",\"name\":\"record2\",\"type\":\"nested\",\"children\":[{\"id\":\"r1\",\"name\":\"record1\",\"type\":\"flat\"}]}">>,
	    FlatRecord = #erlang_record_flat{id="r1", name="record1", type="flat"},
	    NestedRecord = #erlang_record_nested{id="r2", name="record2", type="nested", children=[FlatRecord]},
	    ?assertEqual(NestedRecord, erlang_record_json:to_record(NestedJson, 
								    [#erlang_record_flat{type="flat"}, #erlang_record_nested{type="nested"}],
								    type,
								    erlang_record_json_utils))
    end.
