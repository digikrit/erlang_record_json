-ifndef(log_level).
-define(log_error, 1).
-endif.

-ifdef(log_debug).
-undef(DEBUG).
-define(DEBUG(Format, Args), io:format("[debug] " ++ Format ++ "~n", Args)).
-else.
-define(DEBUG(Format, Args), ok).
-endif.

-ifndef(ERROR_MSG).
-define(ERROR_MSG(Format, Args), io:format("[error] " ++ Format ++ "~n", Args)).
-endif.

-record(erlang_record_flat, {id,
			     name,
			     type,
			     fields}).

-record(erlang_record_nested, {id,
			       name,
			       type,
			       fields,
			       children = [#erlang_record_flat{}]}).
