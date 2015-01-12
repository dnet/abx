-module(abx_transform).
-export([apply_transformation/2]).

-define(TRANSFORM_RE, "^([a-z]+)\\\.([a-z]+)=(.+)$").

apply_transformation(Transformation, Model) ->
	StringPool = proplists:get_value(string_pool, Model),
	CompiledTF = compile_transformation(Transformation),
	ExtendedStringPool = extend_stringpool(StringPool, CompiledTF),
	io:format(standard_error, "TODO ~p~n", [CompiledTF]),
	[{string_pool, ExtendedStringPool} | proplists:delete(string_pool, Model)].

extend_stringpool(Pool, Params) when is_tuple(Params) ->
	extend_stringpool(Pool, lists:filter(fun erlang:is_binary/1, tuple_to_list(Params)));
extend_stringpool(Pool, []) -> Pool;
extend_stringpool(Pool, [Param | Params]) ->
	case lists:member(Param, Pool) of
		true -> extend_stringpool(Pool, Params);
		false -> extend_stringpool(Pool ++ [Param], Params)
	end.

compile_transformation(Transformation) ->
	case re:run(Transformation, ?TRANSFORM_RE,
			[anchored, {capture, all_but_first, binary}, caseless]) of
		{match, [Element, Attribute, Value]} ->
			{Element, Attribute, compile_value(Value)};
		nomatch -> throw({invalid_transformation, Transformation})
	end.

compile_value(<<"true">>) -> true;
compile_value(<<"false">>) -> false;
compile_value(String) ->
	case lists:all(fun (C) -> C =< $9 andalso C >= $0 end, binary_to_list(String)) of
		true -> binary_to_integer(String);
		false -> String
	end.
