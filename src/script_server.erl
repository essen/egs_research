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

-module(script_server).
-behavior(gen_server).
-export([start_link/0, stop/0, func/2, opcode/2, save/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {funcs=[], opcodes=[]}).

%% Use the module name for the server's name.
-define(SERVER, ?MODULE).

%% API.

%% @spec start_link() -> {ok,Pid::pid()}
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec stop() -> stopped
stop() ->
	gen_server:call(?SERVER, stop).

%% @spec func(Name, Func) -> ok
func(Name, Func) ->
	gen_server:call(?SERVER, {func, Name, Func}).

%% @spec opcode(Name, Opcode) -> ok
opcode(Name, Opcode) ->
	gen_server:call(?SERVER, {opcode, Name, Opcode}).

%% @spec save() -> ok
save() ->
	gen_server:call(?SERVER, save, infinity).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({func, Name, Func}, _From, State) ->
	FuncList = State#state.funcs,
	NameList = proplists:get_value(Func, FuncList, []),
	{reply, ok, State#state{funcs=lists:keystore(Func, 1, FuncList, {Func, [Name|NameList]})}};

handle_call({opcode, Name, Opcode}, _From, State) ->
	OpcodeList = State#state.opcodes,
	NameList = proplists:get_value(Opcode, OpcodeList, []),
	NameList1 = case lists:member(Name, NameList) of
		true -> NameList;
		false -> [Name|NameList]
	end,
	{reply, ok, State#state{opcodes=lists:keystore(Opcode, 1, OpcodeList, {Opcode, NameList1})}};

handle_call(save, _From, State) ->
	save_funcs(State#state.funcs),
	save_opcodes(State#state.opcodes),
	{reply, ok, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

save_funcs(Funcs) ->
	[file:write_file("out/funcs.txt", io_lib:format("~s:~s~n", [Func, names_to_str(Names)]), [append]) || {Func, Names} <- Funcs],
	ok.

save_opcodes(Opcodes) ->
	[file:write_file("out/opcodes.txt", io_lib:format("~2.16.0b:~s~n", [Opcode, names_to_str(Names)]), [append]) || {Opcode, Names} <- Opcodes],
	ok.

names_to_str(Names) ->
	lists:flatten([[" ", Name] || Name <- lists:sort(Names)]).
