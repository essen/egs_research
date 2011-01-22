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

-module(script_parser).
-export([parse/2]).

parse(Out, Filename) ->
	Name = string:substr(Filename, 6, string:len(Filename) - 20),
	{ok, Data} = file:read_file(Filename),
	<< $T, $S, $B, $2, FuncsPos:32/little, _FuncsSize:32/little, Rest/binary >> = Data,
	CodeBits = 8 * FuncsPos,
	<< CodeBin:CodeBits/bits, FuncsBin/bits >> = Rest,
	FuncsList = parse_funcs(Name, FuncsBin),
	Script = parse_code(Name, CodeBin, FuncsList),
	file:write_file([Out, "script.es"], Script).

parse_funcs(Name, FuncsBin) ->
	parse_funcs(Name, FuncsBin, []).
parse_funcs(_Name, << >>, Acc) ->
	lists:reverse(Acc);
parse_funcs(Name, FuncsBin, Acc) ->
	<< Func:256/bits, Rest/bits >> = FuncsBin,
	[FuncName|_] = re:split(Func, "\\0\\0", [{return, binary}]),
	script_server:func(Name, FuncName),
	parse_funcs(Name, Rest, [FuncName|Acc]).

parse_code(Name, CodeBin, FuncsList) ->
	parse_code(Name, CodeBin, FuncsList, []).
parse_code(_Name, << >>, _FuncsList, Acc) ->
	lists:reverse(Acc);
parse_code(Name, CodeBin, FuncsList, Acc) ->
	<< _NextPos:32/little, TypeName:256/bits, InstrsSize:32/little, Type:32/little, Rest/bits >> = CodeBin,
	[TypeName1|_] = re:split(TypeName, "\\0\\0", [{return, binary}]),
	{Src, Rest1} = parse_type(Name, Type, TypeName1, InstrsSize, Rest, FuncsList),
	parse_code(Name, Rest1, FuncsList, [Src|Acc]).

parse_type(_Name, 16#3c, TypeName, 0, Data, _FuncsList) ->
	<< 0:32/little, Rest/bits >> = Data,
	{io_lib:format("number ~s~n~n", [TypeName]), Rest};
parse_type(_Name, 16#49, TypeName, 0, Data, _FuncsList) ->
	<< N:32/little, Rest/bits >> = Data,
	{io_lib:format("string ~s (~b)~n~n", [TypeName, N]), Rest};
parse_type(Name, 16#4c, TypeName, InstrsSize, Data, FuncsList) ->
	<< NbVars:32/little, Rest/bits >> = Data,
	InstrsBits = 8 * InstrsSize,
	VarsBits = 64 * NbVars,
	<< InstrsBin:InstrsBits/bits, VarsBin:VarsBits/bits, Rest1/bits >> = Rest,
	VarsList = [{Pos, N} || << N:32/little, Pos:32/little >> <= VarsBin],
	InstrsSrc = parse_instrs(Name, InstrsBin, VarsList, FuncsList),
	{[io_lib:format("function ~s ->~n", [TypeName])|InstrsSrc], Rest1}.

parse_instrs(Name, InstrsBin, VarsList, FuncsList) ->
	parse_instrs(Name, InstrsBin, VarsList, FuncsList, 0, []).
parse_instrs(_Name, << >>, _VarsList, _FuncsList, _Pos, Acc) ->
	lists:flatten(lists:reverse([$\n|Acc]));
parse_instrs(Name, << 16#00:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    return.~n", [])|Acc]);
parse_instrs(Name, << 16#02:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little-signed, Rest2/bits >> = Rest,
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    push ~b,~n", [N])|Acc]);
parse_instrs(Name, << 16#03:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little-float, Rest2/bits >> = Rest,
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    push ~f,~n", [N])|Acc]);
parse_instrs(Name, << 16#04:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    add,~n", [])|Acc]);
parse_instrs(Name, << 16#05:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    sub,~n", [])|Acc]);
parse_instrs(Name, << 16#06:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    mul,~n", [])|Acc]);
parse_instrs(Name, << 16#07:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    div,~n", [])|Acc]);
parse_instrs(Name, << 16#08:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    mod,~n", [])|Acc]);
parse_instrs(Name, << 16#09:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    lshift,~n", [])|Acc]);
parse_instrs(Name, << 16#0a:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    rshift,~n", [])|Acc]);
parse_instrs(Name, << 16#0b:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    band,~n", [])|Acc]);
parse_instrs(Name, << 16#0c:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    bor,~n", [])|Acc]);
parse_instrs(Name, << 16#0d:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    bxor,~n", [])|Acc]);
parse_instrs(Name, << 16#0e:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    inc,~n", [])|Acc]);
parse_instrs(Name, << 16#0f:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    dec,~n", [])|Acc]);
parse_instrs(Name, << 16#10:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    neg,~n", [])|Acc]);
parse_instrs(Name, << 16#11:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    abs,~n", [])|Acc]);
parse_instrs(Name, << 16#12:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_eq,~n", [])|Acc]);
parse_instrs(Name, << 16#13:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_neq,~n", [])|Acc]);
parse_instrs(Name, << 16#14:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_gteq,~n", [])|Acc]);
parse_instrs(Name, << 16#15:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_gt,~n", [])|Acc]);
parse_instrs(Name, << 16#16:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_lteq,~n", [])|Acc]);
parse_instrs(Name, << 16#17:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    is_lt,~n", [])|Acc]);
parse_instrs(Name, << 16#18:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    land,~n", [])|Acc]);
parse_instrs(Name, << 16#19:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    lor,~n", [])|Acc]);
parse_instrs(Name, << 16#1a:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    save,~n", [])|Acc]);
parse_instrs(Name, << 16#1b:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    restore,~n", [])|Acc]);
parse_instrs(Name, << 16#1c:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x1c,~n", [])|Acc]);
parse_instrs(Name, << 16#1d:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    savep,~n", [])|Acc]);
parse_instrs(Name, << 16#1e:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x1e,~n", [])|Acc]);
parse_instrs(Name, << 16#20:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x20,~n", [])|Acc]);
parse_instrs(Name, << 16#23:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x23,~n", [])|Acc]);
parse_instrs(Name, << 16#28:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x28,~n", [])|Acc]);
parse_instrs(Name, << 16#29:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x29,~n", [])|Acc]);
parse_instrs(Name, << 16#2a:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x2a,~n", [])|Acc]);
parse_instrs(Name, << 16#2c:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little-signed, Rest2/bits >> = Rest,
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    jmp ~b,~n", [N])|Acc]);
parse_instrs(Name, << 16#2d:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little-signed, Rest2/bits >> = Rest,
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    jz ~b,~n", [N])|Acc]);
parse_instrs(Name, << 16#2e:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little-signed, Rest2/bits >> = Rest,
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    jnz ~b,~n", [N])|Acc]);
parse_instrs(Name, << 16#3c:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    num_get ~s,~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#3d:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    num_set ~s,~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#46:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< Length:32/little, Rest2/bits >> = Rest,
	StrBits = 32 * Length,
	<< Str:StrBits/bits, Rest3/bits >> = Rest2,
	[Str2|_] = re:split(Str, "\\0\\0", [{return, binary}]),
	parse_instrs(Name, Rest3, VarsList, FuncsList, Pos + 2 + Length, [io_lib:format("    push \"~s\", %% (~b)~n", [Str2, Length])|Acc]);
parse_instrs(Name, << 16#49:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    str_get ~s,~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#4a:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    str_set ~s,~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#4c:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    ~s, %% 0x4c call~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#55:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x55,~n", [])|Acc]);
parse_instrs(Name, << 16#56:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	parse_instrs(Name, Rest, VarsList, FuncsList, Pos + 1, [io_lib:format("    unknown_0x56,~n", [])|Acc]);
parse_instrs(Name, << 16#60:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< 16#ffffffff:32, Rest2/bits >> = Rest,
	FuncNb = proplists:get_value(Pos + 1, VarsList),
	FuncName = lists:nth(FuncNb + 1, FuncsList),
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    ~s, %% 0x60 call~n", [FuncName])|Acc]);
parse_instrs(Name, << 16#61:32/little, Rest/bits >>, VarsList, FuncsList, Pos, Acc) ->
	<< N:32/little, Rest2/bits >> = Rest,
	%% @todo Try to translate the syscall to a name.
	parse_instrs(Name, Rest2, VarsList, FuncsList, Pos + 2, [io_lib:format("    syscall ~b,~n", [N])|Acc]);
parse_instrs(_Name, << Any:32/little, _Rest/bits >>, _VarsList, _FuncsList, _Pos, _Acc) ->
	io:format("Unknown opcode ~2.16.0b, halting...~n", [Any]),
	halt().
