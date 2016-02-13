%% @author Akhil Agrawal <akhil.agrawal@digikrit.com>
%% 
%% json utils for converting erlang record to json and back. As erlang records are compile time definitions, we need to maintain the list of fields
%% using record definitions to allow conversion of erlang records to json and back.
%% 

-module(erlang_record_json).
-vsn("1.0").

%% Log Level
-define(log_level, 1).
-define(log_debug, 1).

-include("logger.hrl").

%% various functions for converting from record to json, from json to record
-export([to_record/3, to_json/3, to_record/4, json_to_record/3, record_to_json/3, to_sanitised_json/3]).

%% converts json to record using the record definitions and field to be matched
to_record(Binary, RecordDefs, SameFieldName, RecordFieldsCallback) ->
    Json = mochijson2:decode(Binary),
    case is_list(Json) of
        true -> lists:map(fun(X) -> json_to_record(X, which_record_def(X, RecordDefs, SameFieldName, RecordFieldsCallback), RecordFieldsCallback) end, Json);
	false -> json_to_record(Json, which_record_def(Json, RecordDefs, SameFieldName, RecordFieldsCallback), RecordFieldsCallback)
    end.

%% converts json to record using the record definition provided
to_record(Binary, RecordDef, RecordFieldsCallback) ->
    Json = mochijson2:decode(Binary),
    ?DEBUG("~p~n", [Json]),
    json_to_record(Json, RecordDef, RecordFieldsCallback).

%% internal function which converts the json tuple from mochijson2 to actual record
json_to_record(RecordJson, RecordDef, RecordFieldsCallback)  ->
    RecordDict = dict:from_list(lists:map(fun({K,V}) ->
						  {binary_to_atom(K, latin1), V} end, element(2, RecordJson))),
    %% ?LOG_INFO("~p", [RecordDict]),
    RecordName = element(1, RecordDef),
    list_to_tuple([RecordName] ++ 
		      lists:zipwith(
			fun(RecordField, FieldDef) ->					  
				FieldValue = case dict:find(RecordField, RecordDict) of
						 {ok, Value} -> Value;
						 error -> undefined
					     end,
				if (FieldValue /= undefined) and (FieldValue /= <<"null">>) and (FieldValue /= "null") and (FieldValue /= null) ->
					case FieldDef of
					    [] -> lists:map(fun(X) -> with_null(X) end, FieldValue);
					    [H|_] -> case is_tuple(H) of
							 true -> lists:map(fun(X) -> json_to_record(X, H, RecordFieldsCallback) end, FieldValue);
							 false -> case is_number(H) or is_atom(H) of
								      true -> with_null(FieldValue);
								      false -> lists:map(fun(X) -> with_null(X) end, FieldValue)
								  end
						     end;
					    _ -> case is_tuple(FieldDef) of
						     true -> json_to_record(FieldValue, FieldDef, RecordFieldsCallback);
						     false -> with_null(FieldValue)
						 end
					end;
				   true -> if is_list(FieldDef) -> [];
					      is_tuple(FieldDef) -> undefined;
					      true -> FieldDef
					   end
				end
			end,
		        RecordFieldsCallback:get_record_fields(RecordName),
			tl(tuple_to_list(RecordDef)))).

%% converts binary to utf list
binary_to_list_utf(Bin) ->
    try
        case unicode:characters_to_list(Bin) of
            L when is_list(L) -> L;
            _ -> binary_to_list(Bin)
        end
    catch
	_:_ -> ?ERROR_MSG("Error while converting binary to list~p~n", [Bin]),
	       []
    end.


with_null(FieldValue) ->
    case FieldValue of
        <<"null">> -> undefined;
	_ -> case is_binary(FieldValue) of
                 true -> binary_to_list_utf(FieldValue);
                 false -> FieldValue
             end
    end.

% find which record def matches the json
which_record_def(Json, RecordDefs, SameFieldName, RecordFieldsCallback) ->
    [RD | _] = RecordDefs,
    SameFieldIndex = 1 + length(lists:takewhile(fun(X) -> case X of
                                                              SameFieldName -> false;
                                                              _ -> true
                                                          end
						end, RecordFieldsCallback:get_record_fields(element(1, RD)))),
    JsonDict = dict:from_list(lists:map(fun({K,V}) ->						
						{binary_to_atom(K, latin1), V} 
					end, element(2, Json))),
    [H | _] = lists:dropwhile(fun(RecordDef) ->
				      case dict:find(SameFieldName, JsonDict) of
                                          {ok, Value} -> FieldValue = element(SameFieldIndex + 1, RecordDef),
							 case binary_to_list_utf(Value) of
							     FieldValue -> false;
							     _ -> true
							 end;
                                          error -> true
                                      end
                              end, RecordDefs),
    H.

%% converts the erlang tuple created from record to binary using mochijson2
to_sanitised_json(Record, RecordDef, RecordFieldsCallback) ->
    mochijson2:encode(record_to_sanitised_json(Record, RecordDef, RecordFieldsCallback)).

%% converts the record to sanitised json (fields with undefined or null values are removed) using the reoord
%% definition provided
record_to_sanitised_json(Record, RecordDef, RecordFieldsCallback) ->    
    case Record of
        undefined -> null;
	_ ->
	    %%?DEBUG("Encoding record ~p with definition ~p~n", [Record, RecordDef]),
            lists:filter(fun({_X,Y}) -> case Y of
					   null -> false;
					   [] -> false;
					   _ -> true
				       end
                         end,
                         lists:zipwith3(fun(RecordField, FieldDef, FieldValue) ->
						{RecordField,
						 case FieldValue of
						     undefined -> null;
						     _ ->
							 case FieldDef of
							     [] -> lists:map(fun(X) -> with_undefined(X) end, FieldValue);
							     [H|_] -> case is_tuple(H) of
									  true -> lists:map(fun(X) -> 
												    record_to_sanitised_json(X, H, RecordFieldsCallback) 
											    end, FieldValue);
									  false -> lists:map(fun(X) -> with_undefined(X) end, FieldValue)
								      end;
							     _ -> case is_tuple(FieldDef) of
								      true -> record_to_sanitised_json(FieldValue, FieldDef, RecordFieldsCallback);
								      false -> with_undefined(FieldValue)
								  end
							 end
						 end}
					end,
					RecordFieldsCallback:get_record_fields(element(1, RecordDef)),
					tl(tuple_to_list(RecordDef)),
					%% we are turning the record into list chopping its head (record name) off
					tl(tuple_to_list(Record))))
    end.

%% converts the erlang tuple created from record to binary using mochijson2
to_json(Record, RecordDef, RecordFieldsCallback) ->
    ErlRec = record_to_json(Record, RecordDef, RecordFieldsCallback),
    ?DEBUG("~p~n", [ErlRec]),
    mochijson2:encode(ErlRec).

%% converts the record to json using the record definition provided
record_to_json(Record, RecordDef, RecordFieldsCallback) ->
    case Record of
        undefined -> null;
	_ ->            
	    %%?DEBUG("Encoding record ~p with definition ~p~n", [Record, RecordDef]),
            lists:zipwith3(
              fun(RecordField, FieldDef, FieldValue) ->
                      {RecordField,
                       case FieldValue of
                           undefined -> null;
                           _ -> case FieldDef of
				    [] -> lists:map(fun(X) -> with_undefined(X) end, FieldValue);
				    [H|_] ->
					case is_tuple(H) of
					    true -> lists:map(fun(X) -> record_to_json(X, H, RecordFieldsCallback) end, FieldValue);
					    false -> lists:map(fun(X) -> with_undefined(X) end, FieldValue)
					end;
				    _ -> case is_tuple(FieldDef) of
					     true -> record_to_json(FieldValue, FieldDef, RecordFieldsCallback);
					     false -> with_undefined(FieldValue)
					 end
				end
                       end}
              end,
              RecordFieldsCallback:get_record_fields(element(1, RecordDef)),
	      tl(tuple_to_list(RecordDef)),
              %% we are turning the record into list chopping its head (record name) off
              tl(tuple_to_list(Record)))
    end.

with_undefined(FieldValue) ->    
    case FieldValue of
        undefined -> null;
	[] -> <<"">>;
        _ -> if is_list(FieldValue) -> case lists:any(fun(X) -> if is_integer(X) and (X < 255) -> true;
								   true -> false
								end
						      end, FieldValue) of
					   true -> unicode:characters_to_binary(FieldValue);
					   false -> FieldValue
				       end;
                is_tuple(FieldValue) -> list_to_tuple(lists:map(fun(X) -> with_undefined(X) end, tuple_to_list(FieldValue))); 
		true -> FieldValue
             end
    end.
