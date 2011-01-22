#	EGS Research: Erlang Game Server Research Suite
#	Copyright (C) 2010  Loic Hoguin
#
#	This file is part of EGS Research.
#
#	EGS Research is free software: you can redistribute it and/or modify
#	it under the terms of the GNU Affero General Public License as
#	published by the Free Software Foundation, either version 3 of the
#	License, or (at your option) any later version.
#
#	EGS Research is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU Affero General Public License for more details.
#
#	You should have received a copy of the GNU Affero General Public License
#	along with EGS Research.  If not, see <http://www.gnu.org/licenses/>.

ERL ?= erl

all: compile

compile: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump
