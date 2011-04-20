%% @author Loïc Hoguin <essen@dev-extend.eu>
%% @copyright 2011 Loïc Hoguin.
%%
%%	This file is part of EGS.
%%
%%	EGS is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU Affero General Public License as
%%	published by the Free Software Foundation, either version 3 of the
%%	License, or (at your option) any later version.
%%
%%	EGS is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU Affero General Public License for more details.
%%
%%	You should have received a copy of the GNU Affero General Public License
%%	along with EGS.  If not, see <http://www.gnu.org/licenses/>.

-module(text_parser).
-export([parse/3]).

parse(Out, Filename, BasePtr) ->
	OutName = string:substr(Filename, 1 + string:rstr(Filename, "/")),
	{ok, Data} = file:read_file(Filename),
	<< Size:32/little, 8:32/little, 12:32/little, PosBin/bits >> = Data,
	Size = byte_size(Data),
	PosList = parse_pos(PosBin, Size, BasePtr, []),
	Strings = parse_strings(Data, PosList, []),
	file:write_file([Out, OutName, ".txt"], [<< 16#fffe:16 >>, Strings]).

parse_pos(<< Pos:32/little, _Rest/bits >>, Size, BasePtr, Acc)
		when Pos - BasePtr < 12; Pos - BasePtr > Size ->
	lists:reverse(Acc);
parse_pos(<< Pos:32/little, Rest/bits >>, Size, BasePtr, Acc) ->
	parse_pos(Rest, Size, BasePtr, [Pos - BasePtr|Acc]).

parse_strings(_Data, [], Acc) ->
	lists:reverse(Acc);
parse_strings(Data, [Pos|Tail], Acc) ->
	<< _Ignore:Pos/binary, Data2/bits >> = Data,
	[String|_] = binary:split(Data2, << 0, 0 >>),
	String2 = binary:replace(String, << $\n, 0 >>, << $~, 0, $n, 0 >>),
	Padding = case byte_size(String2) rem 2 of 1 -> 8; 0 -> 0 end,
	parse_strings(Data, Tail, [<< String2/binary, 0:Padding, $\n, 0 >>|Acc]).
