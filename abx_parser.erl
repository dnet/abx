-module(abx_parser).
-export([parse_file/1, parse_binary/1]).

-define(SP(X), case X of 16#FFFFFFFF -> null; _ -> lists:nth((X) + 1, StringPool) end).
-include("abx.hrl").

parse_file(FileName) ->
	{ok, Contents} = file:read_file(FileName),
	parse_binary(Contents).

parse_binary(<<?RES_XML_TYPE:16/little, 8:16/little,
		ChunkSize:32/little, Rest/binary>> = Chunk) when byte_size(Chunk) =:= ChunkSize ->
	parse_chunks(Rest, []).

parse_chunks(<<>>, Acc) -> lists:reverse(Acc);
parse_chunks(<<ChunkType:16/little, HeaderSize:16/little, ChunkSize:32/little, Rest/binary>>, Acc) ->
	PayloadSize = ChunkSize - 8,
	<<Payload:PayloadSize/binary, Next/binary>> = Rest,
	Acc2 = parse_chunk(ChunkType, HeaderSize, ChunkSize, Payload, Acc),
	parse_chunks(Next, Acc2).

parse_chunk(?RES_STRING_POOL_TYPE, HeaderSize, ChunkSize, Payload, Acc) ->
	[{string_pool, parse_string_pool(HeaderSize, ChunkSize, Payload)} | Acc];
parse_chunk(?RES_XML_START_ELEMENT_TYPE, _HeaderSize, _ChunkSize,
		<<LineNum:32/little, Comment:32/little, NsIndex:32/little, NameIndex:32/little,
			AttrStart:16/little, AttrSize:16/little, AttrCount:16/little,
			_IdClassStyle:6/binary, AttrBytes/binary>>, Acc) ->
	AttrStart = 20,
	AttrSize = 20,
	StringPool = proplists:get_value(string_pool, Acc),
	Attributes = parse_attributes(AttrBytes, AttrCount, StringPool),
	[{element, LineNum, Comment, ?SP(NsIndex), ?SP(NameIndex), Attributes} | Acc];
parse_chunk(Type, _HeaderSize, _ChunkSize, Payload, Acc) ->
	[{unknown, Type, Payload} | Acc].

parse_attributes(Payload, Count, StringPool) -> parse_attributes(Payload, Count, StringPool, []).
parse_attributes(<<>>, 0, _StringPool, Acc) -> lists:reverse(Acc);
parse_attributes(<<NsIndex:32/little, NameIndex:32/little, RawValue:32/little,
		TypedValue:8/binary, Rest/binary>>, Count, StringPool, Acc) ->
	Value = case RawValue of
		16#FFFFFFFF -> parse_typed_value(TypedValue);
		_ -> ?SP(RawValue)
	end,
	Attribute = {?SP(NsIndex), ?SP(NameIndex), Value},
	parse_attributes(Rest, Count - 1, StringPool, [Attribute | Acc]).

parse_typed_value(<<8:16/little, 0, ?TYPE_INT_BOOLEAN, 0:32/little>>) -> false;
parse_typed_value(<<8:16/little, 0, ?TYPE_INT_BOOLEAN, _:32>>) -> true;
parse_typed_value(<<8:16/little, 0, ?TYPE_FIRST_INT, Ref:32/little>>) -> Ref;
parse_typed_value(<<8:16/little, 0, ?TYPE_INT_HEX, Ref:32/little>>) -> {hex, Ref};
parse_typed_value(<<8:16/little, 0, ?TYPE_REFERENCE, Ref:32/little>>) -> {ref, Ref}.

parse_string_pool(HeaderSize, ChunkSize, <<StringCount:32/little, StyleCount:32/little, Flag:32/little, StringStart:32/little, StyleStart:32/little, Rest/binary>>) ->
	StringIndicesByteCount = StringCount * 4,
	StyleByteCount = StyleCount * 4,
	Flag = 0,
	StringStart = HeaderSize + StringIndicesByteCount + StyleByteCount,
	<<StringIndices:StringIndicesByteCount/binary, _Styles:StyleByteCount/binary, Strings/binary>> = Rest,
	parse_strings(StringIndices, Strings, {HeaderSize, ChunkSize, StringCount, StyleStart}).

parse_strings(StringIndices, Strings, Params) ->
	{Result, <<>>} = parse_strings(StringIndices, Strings, [], Params),
	lists:reverse(Result).

parse_strings(<<Index:32/little>>, Strings, Acc, {HeaderSize, ChunkSize, StringCount, 0}) ->
	StringLen = ChunkSize - Index - HeaderSize - 4 * StringCount,
	parse_string(StringLen, Strings, Acc);
parse_strings(<<Index:32/little>>, Strings, Acc, {_, _, _, StyleStart}) ->
	StringLen = StyleStart - Index,
	parse_string(StringLen, Strings, Acc);
parse_strings(<<Index:32/little, NextIndex:32/little, Indices/binary>>, Strings, Acc, Params) ->
	StringLen = NextIndex - Index,
	{Acc2, Strings2} = parse_string(StringLen, Strings, Acc),
	parse_strings(<<NextIndex:32/little, Indices/binary>>, Strings2, Acc2, Params).

parse_string(Length, <<Len1, Len2, Payload/binary>>, Acc) ->
	ActualLength = case Len1 of
		Len2 -> Len2;
		_ -> Len1 bor (Len2 bsl 8)
	end,
	BinaryLength = Length - 2,
	<<String:BinaryLength/binary, Rest/binary>> = Payload,
	Result = binary:replace(String, <<0>>, <<>>, [global]),
	ActualLength = byte_size(Result),
	{[Result | Acc], Rest}.
