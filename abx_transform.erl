-module(abx_transform).
-export([apply_transformation/2]).

-define(TRANSFORM_RE, "^(?:{([^}]+)})?([a-z]+)\\\.([a-z]+)=(.+)$").
-define(DEFAULT_NS, <<"http://schemas.android.com/apk/res/android">>).

-record(transform, {ns=?DEFAULT_NS, tag, attr, value}).

apply_transformation(Transformation, Model) ->
	T = compile_transformation(Transformation),
	[transform_chunk(T, Chunk) || Chunk <- Model].

transform_chunk(Transform, {string_pool, StringPool}) ->
	{string_pool, extend_stringpool(StringPool, Transform)};
transform_chunk(#transform{tag=Name}=T, {element, _, _, _, Name, OldAttr}=E) ->
	setelement(6, E, transform_attributes(T, OldAttr));
transform_chunk(_, Chunk) -> Chunk.

transform_attributes(T, A) -> transform_attributes(T, A, []).
transform_attributes(#transform{ns=Ns, attr=Attr, value=Value}, [], Acc) ->
	lists:reverse(Acc, [{Ns, Attr, Value}]);
transform_attributes(#transform{ns=Ns, tag=Name, value=NewValue},
		[{Ns, Name, _}=A | Attributes], Acc) ->
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
		{match, [<<>>, Tag, Attr, Value]} ->
			#transform{tag=Tag, attr=Attr, value=compile_value(Value)};
		{match, [Ns, Tag, Attr, Value]} ->
			#transform{ns=Ns, tag=Tag, attr=Attr, value=compile_value(Value)};
		nomatch -> throw({invalid_transformation, Transformation})
	end.

compile_value(<<"true">>) -> true;
compile_value(<<"false">>) -> false;
compile_value(String) ->
	case lists:all(fun (C) -> C =< $9 andalso C >= $0 end, binary_to_list(String)) of
		true -> binary_to_integer(String);
		false -> String
	end.
