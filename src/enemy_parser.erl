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

-module(enemy_parser).
-export([parse/4]).

parse(Out, Filename, Shortname, BasePtr) ->
	{ok, Data} = file:read_file(Filename),
	<< $N, $X, $R, 0, _EndRelPtr:32/little, HeadRelPtr:32/little, 0:32, _/bits >> = Data,
	<< _:HeadRelPtr/binary, SpawnsPtr:32/little, NbSpawns:32/little, _/bits >> = Data,
	SpawnsList = parse_spawns(Data, BasePtr, NbSpawns, SpawnsPtr - BasePtr),
	file:write_file([Out, Shortname, ".conf"], io_lib:format("~p~n", [SpawnsList])).

parse_spawns(Data, BasePtr, NbSpawns, SpawnsPtr) ->
	parse_spawns(Data, BasePtr, NbSpawns, SpawnsPtr, []).
parse_spawns(Data, BasePtr, 0, SpawnsPtr, Acc) ->
	lists:reverse(Acc);
parse_spawns(Data, BasePtr, NbSpawns, SpawnsPtr, Acc) ->
	<<	_:SpawnsPtr/binary, SpawnPtr:32/little, 1:32/little,
		SpawnInfoPtr:32/little, NbSpawnInfos:32/little,
		SpawnEnemiesPtr:32/little, NbSpawnEnemies:32/little, _/bits >> = Data,
	SpawnNb = parse_spawn(Data, SpawnPtr - BasePtr),
	Infos = parse_infos(Data, BasePtr, NbSpawnInfos, SpawnInfoPtr - BasePtr),
	Enemies = parse_enemies_header(Data, BasePtr, NbSpawnEnemies, SpawnEnemiesPtr - BasePtr),
	Spawn = {SpawnNb, {infos, Infos}, {enemies, Enemies}},
	parse_spawns(Data, BasePtr, NbSpawns - 1, SpawnsPtr + 24, [Spawn|Acc]).

parse_spawn(Data, SpawnPtr) ->
	<<	_:SpawnPtr/binary,
		SpawnNb:16/little, UnknownA:16/little,
		UnknownB:16/little, 0:48, _/bits >> = Data,
	SpawnNb.

parse_infos(Data, BasePtr, NbSpawnInfos, InfosPtr) ->
	parse_infos(Data, BasePtr, NbSpawnInfos, InfosPtr, []).
parse_infos(Data, BasePtr, 0, InfosPtr, Acc) ->
	lists:reverse(Acc);
parse_infos(Data, BasePtr, NbSpawnInfos, InfosPtr, Acc) ->
	<<	_:InfosPtr/binary,
		UnknownZ:16/little, UnknownA:16/little,
		UnknownB:16/little, MaxEnemies:16/little,
		TriggerCount:16/little, UnknownC:16/little,
		0:32, _/bits >> = Data,
	Infos = [{max_enemies, MaxEnemies}, {trigger_count, TriggerCount}],
	parse_infos(Data, BasePtr, NbSpawnInfos - 1, InfosPtr + 16, [Infos|Acc]).

parse_enemies_header(Data, BasePtr, NbHeaders, HeaderPtr) ->
	parse_enemies_header(Data, BasePtr, NbHeaders, HeaderPtr, []).
parse_enemies_header(Data, BasePtr, 0, HeaderPtr, Acc) ->
	lists:reverse(Acc);
parse_enemies_header(Data, BasePtr, NbHeaders, HeaderPtr, Acc) ->
	<< _:HeaderPtr/binary, EnemiesPtr:32/little, NbEnemies:32/little, _/bits >> = Data,
	Enemies = parse_enemies(Data, BasePtr, NbEnemies, EnemiesPtr - BasePtr),
	parse_enemies_header(Data, BasePtr, NbHeaders - 1, HeaderPtr + 8, [Enemies|Acc]).

parse_enemies(Data, BasePtr, NbEnemies, EnemiesPtr) ->
	parse_enemies(Data, BasePtr, NbEnemies, EnemiesPtr, []).
parse_enemies(Data, BasePtr, 0, EnemiesPtr, Acc) ->
	lists:reverse(Acc);
parse_enemies(Data, BasePtr, NbEnemies, EnemiesPtr, Acc) ->
	<<	_:EnemiesPtr/binary,
		Species:16/little, Element:16/little,
		BuffKing, BuffShield, BuffSword, UnknownA,
		BuffMagicFoot, 0, UnknownB:16/little,
		UnknownC:16/little, Delay:16/little,
		Count:16/little, 0:16/little,
		UnknownD:32,
		LevelMod:16/little-signed, MaxLevel:16/little,
		UnknownE:16/little, UnknownF, 0,
		0:32, _/bits >> = Data,
	%% UnknownA: 0 or 1
	%% UnknownB: 1, 2 or 3
	%% UnknownC: 1 or 3
	%% UnknownD: 0 or huge value
	%% UnknownE: 0, 2 or 10
	%% UnknownF: 0 or 1
	Enemies = [
		{species, Species},
		{element, Element},
		{buff_king, BuffKing},
		{buff_shield, BuffShield},
		{buff_sword, BuffSword},
		{buff_magic_foot, BuffMagicFoot},
		{delay, Delay},
		{count, Count},
		{level_mod, LevelMod},
		{max_level, MaxLevel}],
	parse_enemies(Data, BasePtr, NbEnemies - 1, EnemiesPtr + 36, [Enemies|Acc]).
