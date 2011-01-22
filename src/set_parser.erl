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

-module(set_parser).
-export([parse/3]).

%% @todo _EndRelPtr?
parse(Out, Filename, BasePtr) ->
	OutName = string:substr(Filename, 1 + string:rstr(Filename, "/"), 6),
	{ok, Data} = file:read_file(Filename),
	<< $N, $X, $R, 0, _EndRelPtr:32/little, AreaIDRelPtr:32/little, 0:32, _/bits >> = Data,
	<< _:AreaIDRelPtr/binary, _AreaID:16/little, NbMaps:16/little, MapsPtr:32/little, _/bits >> = Data,
	ObjList = parse_maps(Data, BasePtr, NbMaps, MapsPtr - BasePtr),
	file:write_file([Out, OutName, ".conf"], io_lib:format("~p~n", [ObjList])).

parse_maps(Data, BasePtr, NbMaps, MapsPtr) ->
	parse_maps(Data, BasePtr, NbMaps, MapsPtr, []).
parse_maps(_Data, _BasePtr, 0, _MapsPtr, Acc) ->
	lists:reverse(Acc);
parse_maps(Data, BasePtr, NbMaps, MapsPtr, Acc) ->
	<< _:MapsPtr/binary, MapID:16/little, NbGroups:16/little, GroupsPtr:32/little, 0:32, _/bits >> = Data,
	Groups = parse_groups(Data, BasePtr, NbGroups, GroupsPtr - BasePtr),
	parse_maps(Data, BasePtr, NbMaps - 1, MapsPtr + 12, [{{map, MapID}, Groups}|Acc]).

parse_groups(Data, BasePtr, NbGroups, GroupsPtr) ->
	parse_groups(Data, BasePtr, NbGroups, GroupsPtr, []).
parse_groups(_Data, _BasePtr, 0, _GroupsPtr, Acc) ->
	lists:reverse(Acc);
parse_groups(Data, BasePtr, NbGroups, GroupsPtr, Acc) ->
	<<	_:GroupsPtr/binary, 16#ffffffff:32, _UnknownA:144/bits, _UnknownB:16/little, _UnknownC:32/little,
		_GroupNumber:16/little, 0:32, NbObjects:16/little, ObjectsPtr:32/little, _/bits >> = Data,
	Objects = parse_objects(Data, BasePtr, NbObjects, ObjectsPtr - BasePtr),
	parse_groups(Data, BasePtr, NbGroups - 1, GroupsPtr + 40, [Objects|Acc]).

parse_objects(Data, BasePtr, NbObjects, ObjectsPtr) ->
	parse_objects(Data, BasePtr, NbObjects, ObjectsPtr, []).
parse_objects(_Data, _BasePtr, 0, _ObjectsPtr, Acc) ->
	lists:reverse(Acc);
parse_objects(Data, BasePtr, NbObjects, ObjectsPtr, Acc) ->
	<<	_:ObjectsPtr/binary, 16#ffffffff:32, ClassID:32/little,
		16#ffffffff:32, 16#ffff:16, TypeID:16/little, _UnknownA:32/little-float,
		PosX:32/little-float, PosY:32/little-float, PosZ:32/little-float,
		RotX:32/little-float, RotY:32/little-float, RotZ:32/little-float,
		ArgsSize:32/little, ArgsPtr:32/little, _/bits >> = Data,
	ArgsPtr1 = ArgsPtr - BasePtr,
	<<	_:ArgsPtr1/binary, ArgsBin:ArgsSize/binary, _/bits >> = Data,
	{Name, Args} = parse_args(TypeID, ClassID, ArgsBin),
	parse_objects(Data, BasePtr, NbObjects - 1, ObjectsPtr + 52,
		[{Name, {PosX, PosY, PosZ}, {RotX, RotY, RotZ}, Args}|Acc]).

parse_args( 4, 4, ArgsBin) -> %% Static model.
	{static_model, ArgsBin};
parse_args( 5, 5, ArgsBin) -> %% Floor button.
	{floor_button, ArgsBin};
parse_args( 6, 2, ArgsBin) -> %% Fog.
	{fog, ArgsBin};
parse_args( 9, 4, ArgsBin) -> %% Sensor.
	{sensor, ArgsBin};
parse_args(10, 1, ArgsBin) -> %% Invisible block.
	{invisible_block, ArgsBin};
parse_args(12, 6, ArgsBin) -> %% Box.
	{box, ArgsBin};
parse_args(14, 1, ArgsBin) -> %% Warp.
	{warp, ArgsBin};
parse_args(17, 2, ArgsBin) -> %% Fence.
	{fence, ArgsBin};
parse_args(18, 2, ArgsBin) -> %% NPC.
	{npc, ArgsBin};
parse_args(20, 5, ArgsBin) -> %% Door.
	{door, ArgsBin};
parse_args(22, 3, ArgsBin) -> %% Key console.
	{key_console, ArgsBin};
parse_args(23, 2, ArgsBin) -> %% Small spawn.
	{spawn10, ArgsBin};
parse_args(24, 2, ArgsBin) -> %% Big spawn.
	{spawn30, ArgsBin};
parse_args(26, 2, ArgsBin) -> %% Entrance.
	{entrance, ArgsBin};
parse_args(27, 6, ArgsBin) -> %% Exit.
	{exit, ArgsBin};
parse_args(28, 0, ArgsBin) -> %% @todo ?
	{unknown_28, ArgsBin};
parse_args(29, 1, ArgsBin) -> %% @todo ?
	{unknown_29, ArgsBin};
parse_args(31, 3, ArgsBin) -> %% Key.
	{key, ArgsBin};
parse_args(33, 0, ArgsBin) -> %% @todo ?
	{unknown_33, ArgsBin};
parse_args(35, 2, ArgsBin) -> %% Boss.
	{boss, ArgsBin};
parse_args(37, 1, ArgsBin) -> %% @todo ?
	{unknown_37, ArgsBin};
parse_args(39, 5, ArgsBin) -> %% @todo ?
	{unknown_39, ArgsBin};
parse_args(40, 2, ArgsBin) -> %% Save sphere.
	{save_sphere, ArgsBin};
parse_args(42, 6, ArgsBin) -> %% @todo Targetable elements in your room?
	{unknown_42, ArgsBin};
parse_args(43, 4, ArgsBin) -> %% Shoot button.
	{shoot_button, ArgsBin};
parse_args(44, 9, ArgsBin) -> %% Trap. @todo Trap type?
	{trap_44, ArgsBin};
parse_args(45, 1, ArgsBin) -> %% Shop counter.
	{shop_counter, ArgsBin};
parse_args(47, 1, ArgsBin) -> %% Type counter.
	{type_counter, ArgsBin};
parse_args(48, 3, ArgsBin) -> %% Boss gate.
	{boss_gate, ArgsBin};
parse_args(49, 3, ArgsBin) -> %% Crystal.
	{crystal, ArgsBin};
parse_args(50, 2, ArgsBin) -> %% Healing pad.
	{healing_pad, ArgsBin};
parse_args(51, 5, ArgsBin) -> %% Goggles target.
	{goggles_target, ArgsBin};
parse_args(52, 3, ArgsBin) -> %% SEED Blewme.
	{seed_blewme, ArgsBin};
parse_args(53, 2, ArgsBin) -> %% Label.
	{label, ArgsBin};
parse_args(54, 2, ArgsBin) -> %% SEED Zoma.
	{seed_zoma, ArgsBin};
parse_args(55, 1, ArgsBin) -> %% Photon Spot.
	{photon_spot, ArgsBin};
parse_args(56, 2, ArgsBin) -> %% Chair.
	{chair, ArgsBin};
parse_args(57, 1, ArgsBin) -> %% Vehicle boost.
	{vehicle_boost, ArgsBin};
parse_args(58, 0, ArgsBin) -> %% Vehicle.
	{vehicle, ArgsBin};
parse_args(59, 1, ArgsBin) -> %% Poster.
	{poster, ArgsBin};
parse_args(60, 0, ArgsBin) -> %% Uni cube.
	{uni_cube, ArgsBin};
parse_args(61, 0, ArgsBin) -> %% Ghosts generator.
	{ghosts_generator, ArgsBin};
parse_args(62, 0, ArgsBin) -> %% PP cube.
	{pp_cube, ArgsBin};
parse_args(63, 0, ArgsBin) -> %% @todo Dressing room entrance?
	{unknown_63, ArgsBin};
parse_args(64, 0, ArgsBin) -> %% Colored minimap section.
	{colored_minimap_section, ArgsBin};
parse_args(65, 4, ArgsBin) -> %% Room decoration slot.
	{room_decoration_slot, ArgsBin};
parse_args(66, 3, ArgsBin) -> %% @todo Offering box at shitenkaku?
	{unknown_66, ArgsBin};
parse_args(67, 3, ArgsBin) -> %% @todo Casino bets and 2 ladies?
	{unknown_67, ArgsBin};
parse_args(68, 4, ArgsBin) -> %% Casino slot machine.
	{casino_slot_machine, ArgsBin};
parse_args(69, 1, ArgsBin) -> %% @todo ?
	{unknown_69, ArgsBin};
parse_args(70, 0, ArgsBin) -> %% Trap. @todo Trap type?
	{trap_70, ArgsBin};
parse_args(71, 0, ArgsBin) -> %% Trap. @todo Trap type?
	{trap_71, ArgsBin};
parse_args(TypeID, ClassID, _ArgsBin) ->
	io:format("unknown object ~b of class ~b~n", [TypeID, ClassID]).
