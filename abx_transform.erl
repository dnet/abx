-module(abx_transform).
-export([apply_transformation/2]).

-define(TRANSFORM_RE, "^([a-z]+)\\\.([a-z]+)=(.+)$").

apply_transformation(Transformation, Model) ->
	walk_model(compile_transformation(Transformation), Model).

walk_model(T, M) -> walk_model(T, M, []).
walk_model(_T, [], Acc) -> lists:reverse(Acc);
walk_model(Transform, [Chunk | Model], Acc) ->
	NewChunk = transform_chunk(Transform, Chunk),
	walk_model(Transform, Model, [NewChunk | Acc]).

transform_chunk(Transform, {string_pool, StringPool}) ->
	{string_pool, extend_stringpool(StringPool, Transform)};
transform_chunk({Name, _, _}=T, {element, _, _, _, Name, OldAttr}=E) ->
	setelement(6, E, transform_attributes(T, OldAttr));
transform_chunk(_, Chunk) -> Chunk.

transform_attributes(T, A) -> transform_attributes(T, A, []).
transform_attributes({_, N, V}, [], Acc) ->
	NewAttr = {<<"http://schemas.android.com/apk/res/android">>, N, V},
	lists:reverse(Acc, [NewAttr]);
transform_attributes({_, Name, NewValue}, [{_, Name, _}=A | Attributes], Acc) ->
	lists:reverse(Attributes, [setelement(3, A, NewValue) | Acc]);
transform_attributes(T, [Attribute | Attributes], Acc) ->
	transform_attributes(T, Attributes, [Attribute | Acc]).


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
