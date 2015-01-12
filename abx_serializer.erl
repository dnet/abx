-module(abx_serializer).
-export([serialize_to_file/2, serialize_to_binary/1]).

-include("abx.hrl").

serialize_to_file(FileName, Chunks) ->
	file:write_file(FileName, serialize_to_binary(Chunks)).

serialize_to_binary(Chunks) ->
	StringPool = proplists:get_value(string_pool, Chunks),
	put(string_pool, dict:from_list(lists:zip(StringPool,
		lists:seq(0, length(StringPool) - 1)))),
	Chunk = serialize_chunks(Chunks),
	ChunkSize = byte_size(Chunk) + 8,
	<<?RES_XML_TYPE:16/little, 8:16/little, ChunkSize:32/little, Chunk/binary>>.

serialize_chunks(Chunks) -> serialize_chunks(Chunks, <<>>).
serialize_chunks([], Acc) -> Acc;
serialize_chunks([Chunk | Chunks], Acc) ->
	{Type, HeaderSize, Serialized} = serialize_chunk(Chunk),
	FullSize = byte_size(Serialized) + 8,
	PadBytes = case FullSize rem 4 of 0 -> 0; R -> 4 - R end,
	Padding = binary:copy(<<0>>, PadBytes),
	serialize_chunks(Chunks, <<Acc/binary, Type:16/little, HeaderSize:16/little,
		(FullSize + PadBytes):32/little, Serialized/binary, Padding/binary>>).

serialize_chunk({string_pool, Pool}) ->
	{?RES_STRING_POOL_TYPE, 28, serialize_string_pool(Pool)};
serialize_chunk({element, LineNum, Comment, Namespace, Name, Attributes}) ->
	AttrStart = 20,
	AttrSize = 20,
	AttrCount = length(Attributes),
	NsIndex = query_string_pool(Namespace),
	NameIndex = query_string_pool(Name),
	AttrBytes = << <<(serialize_attribute(A))/binary>> || A <- Attributes >>,
	{?RES_XML_START_ELEMENT_TYPE, 16, <<LineNum:32/little, Comment:32/little,
		NsIndex:32/little, NameIndex:32/little, AttrStart:16/little,
		AttrSize:16/little, AttrCount:16/little, 0:48, AttrBytes/binary>>};
serialize_chunk({unknown, Type, HeaderSize, Payload}) -> {Type, HeaderSize, Payload}.

query_string_pool(null) -> 16#FFFFFFFF;
query_string_pool(String) -> dict:fetch(String, get(string_pool)).

serialize_attribute({Namespace, Name, Value}) ->
	EncodedValue = serialize_attribute_value(Value),
	NsIndex = query_string_pool(Namespace),
	NameIndex = query_string_pool(Name),
	<<NsIndex:32/little, NameIndex:32/little, EncodedValue/binary>>.

serialize_attribute_value(true) ->
	<<16#FFFFFFFF:32/little, 8:16/little, 0, ?TYPE_INT_BOOLEAN, 16#FFFFFFFF:32>>;
serialize_attribute_value(false) ->
	<<16#FFFFFFFF:32/little, 8:16/little, 0, ?TYPE_INT_BOOLEAN, 0:32>>;
serialize_attribute_value({hex, Ref}) ->
	<<16#FFFFFFFF:32/little, 8:16/little, 0, ?TYPE_INT_HEX, Ref:32/little>>;
serialize_attribute_value({ref, Ref}) ->
	<<16#FFFFFFFF:32/little, 8:16/little, 0, ?TYPE_REFERENCE, Ref:32/little>>;
serialize_attribute_value(Number) when is_integer(Number) ->
	<<16#FFFFFFFF:32/little, 8:16/little, 0, ?TYPE_INT_DECIMAL, Number:32/little>>;
serialize_attribute_value(String) when is_binary(String) ->
	PoolId = query_string_pool(String),
	<<PoolId:32/little, (serialize_string_attribute(String, PoolId))/binary>>.

serialize_string_attribute(String, PoolId) ->
	case lists:all(fun (C) -> C =< $9 andalso C >= $0 end, binary_to_list(String)) of
		true -> <<8:16/little, 0, ?TYPE_INT_DECIMAL, (binary_to_integer(String)):32/little>>;
		false -> <<8:16/little, 0, ?TYPE_STRING, PoolId:32/little>>
	end.

serialize_string_pool(Strings) ->
	{Indices, Pool} = serialize_strings(Strings),
	StringStart = 28 + byte_size(Indices),
	<<(length(Strings)):32/little, 0:64, StringStart:32/little, 0:32,
		Indices/binary, Pool/binary>>.

serialize_strings(Strings) -> serialize_strings(Strings, <<>>, <<>>).
serialize_strings([], Indices, Pool) -> {Indices, Pool};
serialize_strings([String | Strings], Indices, Pool) ->
	Index = byte_size(Pool),
	Serialized = serialize_string(String),
	serialize_strings(Strings, <<Indices/binary, Index:32/little>>,
		<<Pool/binary, Serialized/binary>>).

serialize_string(String) ->
	Payload = << <<Char, 0>> || <<Char>> <= String>>,
	<<(byte_size(String)):16/little, Payload/binary, 0:16>>.
