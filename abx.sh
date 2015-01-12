#!/usr/bin/env escript

main([InputFileName, OutputFileName | Transformations]) ->
	Input = abx_parser:parse_file(InputFileName),
	Output = lists:foldl(fun abx_transform:apply_transformation/2, Input, Transformations),
	abx_serializer:serialize_to_file(OutputFileName, Output);
main([FileName]) ->
	self_test(FileName);
main(_) ->
	io:format(standard_error, "Usage: ./abx.sh input.xml   # self-test regarding that XML~n", []),
	halt(1).

self_test(FileName) ->
	{ok, XML} = file:read_file(FileName),
	case abx_serializer:serialize_to_binary(abx_parser:parse_binary(XML)) of
		XML -> io:format(standard_error,
			"\033[32mSelf-test succeeded, proceed with injection\033[0m~n", []);
		_ -> io:format(standard_error,
			"\033[31mSelf-test failed, be careful with injection\033[0m~n", []),
			halt(1)
	end.
