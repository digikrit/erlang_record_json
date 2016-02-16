%% @author Akhil Agrawal <akhil.agrawal@digikrit.com>
%% json utils for converting erlang record to json and back. As erlang records are compile time definitions, we need to maintain the list of fields
%% using record definitions to allow conversion of erlang records to json and back.
%%

-module(erlang_record_json_utils).
-vsn("1.0").

-include("erlang_record_json.hrl").

%% initialize and get the record fields which will be needed for json
-export([init_record_fields/0, get_record_fields/1, terminate_record_fields/0]).

%% initialize the record fields for any record which needs to be converted to json
init_record_fields() ->    
    ets:new(erlang_record_json_utils_records, [named_table]),
    %% records
    ets:insert(erlang_record_json_utils_records, {erlang_record_flat, record_info(fields, erlang_record_flat)}),    
    ets:insert(erlang_record_json_utils_records, {erlang_record_nested, record_info(fields, erlang_record_nested)}).    

%% get the record fields for a particular record
get_record_fields(RecordName) ->    
    case ets:lookup(erlang_record_json_utils_records, RecordName) of
        [H | _] ->
	    element(2, H);
	[] -> ?ERROR_MSG("Record name ~p missing in module ~p~n", [RecordName, ?MODULE]),
              []
    end.

terminate_record_fields() ->
    ets:delete(erlang_record_json_utils_records).
