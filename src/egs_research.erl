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

-module(egs_research).
-export([start/0, parse_zones/0, parse_zone/1]).

%% @doc Start the research suite.
start() ->
	filelib:ensure_dir("out/"),
	script_server:start_link(),
	parse_zones(),
	script_server:save(),
	script_server:stop().

%% @doc Parse all the available zone files.
parse_zones() ->
	Filenames = rec_list_dir("priv/zones"),
	plists:foreach(fun(Filename) -> parse_zone(Filename) end, Filenames, [{processes, 25}]).

%% @doc Parse a single zone file.
parse_zone(Filename) ->
	NblFilenames = nbl_list_files(Filename),
	Dir = nbl_extract_files(Filename),
	Out = string:substr(binary_to_list(Filename), 6),
	filelib:ensure_dir(["out/", Out, $/]),
	parse_zone_files(["out/", Out, $/], Dir, NblFilenames),
	nbl_cleanup(Filename).

%% @doc Parse the files contained in a zone nbl archive.
parse_zone_files(Out, Dir, NblFilenames) ->
	parse_zone_files(Out, Dir, NblFilenames, 0).
parse_zone_files(_Out, _Dir, [], _Ptr) ->
	ok;
parse_zone_files(Out, Dir, [Filename|Tail], Ptr) when Filename =:= "script.bin" ->
	ExpFilename = exp_file([Dir|Filename]),
	script_parser:parse(Out, ExpFilename),
	parse_zone_files(Out, Dir, Tail, Ptr + calc_padded_size([Dir|Filename]));
parse_zone_files(Out, Dir, [Filename|Tail], Ptr)
		when Filename =:= "set_r0.rel";
			 Filename =:= "set_r1.rel";
			 Filename =:= "set_r2.rel";
			 Filename =:= "set_r3.rel" ->
	set_parser:parse(Out, lists:flatten([Dir|Filename]), Ptr),
	parse_zone_files(Out, Dir, Tail, Ptr + calc_padded_size([Dir|Filename]));
parse_zone_files(Out, Dir, [Filename|Tail], Ptr) ->
	io:format("zone: ignored file ~s from directory ~s~n", [Filename, Dir]),
	parse_zone_files(Out, Dir, Tail, Ptr + calc_padded_size([Dir|Filename])).

%% Utility functions.

%% @doc Calculate the padded file size for Filename.
calc_padded_size(Filename) ->
	FileSize = filelib:file_size(Filename),
	case FileSize rem 32 of
		0 -> FileSize;
		_ -> 32 * (1 + (FileSize div 32))
	end.

%% @doc Expand the given compressed file.
exp_file(Filename) ->
	Cmd = case os:type() of
		{win32, nt} -> lists:flatten(io_lib:format("exp ~s", [Filename]));
		_ -> io_lib:format("./exp ~s", [Filename])
	end,
	os:cmd(Cmd),
	lists:flatten([Filename|".exp"]).

%% @doc List the files contained inside an nbl archive.
nbl_list_files(NblFilename) ->
	{Cmd, NL} = case os:type() of
		{win32, nt} -> {lists:flatten(io_lib:format("nbl -t ~s", [NblFilename])), "\r\n"};
		_ -> {io_lib:format("./nbl -t ~s", [NblFilename]), "\n"}
	end,
	StdOut = os:cmd(Cmd),
	lists:delete([], re:split(StdOut, NL, [{return, list}])).

%% @doc Extract the files from an nbl archive into a temporary directory and return the directory path.
nbl_extract_files(NblFilename) ->
	NblFilename1 = binary_to_list(NblFilename),
	Dir = io_lib:format("/tmp/~s/", [string:sub_string(NblFilename1, 1 + string:rchr(NblFilename1, $/))]),
	filelib:ensure_dir(Dir),
	Cmd = case os:type() of
		{win32, nt} -> lists:flatten(io_lib:format("nbl -o ~s ~s", [Dir, NblFilename]));
		_ -> io_lib:format("./nbl -o ~s ~s", [Dir, NblFilename])
	end,
	os:cmd(Cmd),
	Dir.

%% @doc Cleanup the temporary directory created for the given nbl archive.
nbl_cleanup(NblFilename) ->
	NblFilename1 = binary_to_list(NblFilename),
	Dir = io_lib:format("/tmp/~s/", [string:sub_string(NblFilename1, 1 + string:rchr(NblFilename1, $/))]),
	{ok, Filenames} = file:list_dir(Dir),
	[file:delete(Dir ++ Filename) || Filename <- Filenames],
	file:del_dir(Dir).

%% @doc Return a list of full paths for all files in a given directory.
rec_list_dir(Dir) ->
	Dir1 = [Dir|"/"],
	{ok, Filenames} = file:list_dir(Dir1),
	Filenames1 = [case filelib:is_dir([Dir1|Filename]) of
		true -> rec_list_dir([Dir1|Filename]);
		false -> iolist_to_binary([Dir1|Filename])
	end || Filename <- Filenames, hd(Filename) =/= $.],
	lists:sort(lists:flatten(Filenames1)).
