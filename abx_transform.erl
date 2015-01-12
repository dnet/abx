-module(abx_transform).
-export([apply_transformation/2]).

-define(TRANSFORM_RE, "^([a-z]+)\\\.([a-z]+)=(.+)$").

apply_transformation(Transformation, Model) ->
	io:format(standard_error, "TODO ~p~n", [compile_transformation(Transformation)]),
	Model.

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
