%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(epasswd_mod_htpasswd).

-behaviour(epasswd).

%%%
%%% epasswd callbacks
%%%
-export([init/1,
	 auth/2]).

-define(NOT_LOADED, not_loaded(?LINE)).
-on_load(load/0).

-record(state, {db, path}).
-type state() :: #state{}.

-spec init(Path :: binary()) -> state().
init(Path) ->
    application:start(crypto),
    case file:open(Path, [read, raw, binary, {read_ahead, 512}]) of
	{ok, File} ->
	    Db = parse(file:read_line(File), File, gb_trees:empty()),
	    #state{db=Db};
	{error, Err} ->
	    throw({error, Err})
    end.

-spec auth({User :: iolist(), Passwd :: iolist()}, state()) -> {true | false, state()}.
auth({User, Passwd}, State) when is_list(User) ->
    auth({list_to_binary(User), Passwd}, State);
auth({User, Passwd}, #state{db=Db}=State) ->
    case gb_trees:lookup(User, Db) of
	{value, Digest} ->
	    P = if is_binary(Passwd) -> binary_to_list(Passwd);
		   true -> Passwd
		end,
	    try validate_password(P, binary_to_list(Digest)) of
		true -> {true, State};
		false -> {false, State}
	    catch _:Err ->
		    throw({error, Err})
	    end;
	_ ->
	    {false, State}
    end.

%%%
%%% NIF
%%%
-spec validate_password(Passwd :: string(), Hash :: string()) -> true | false.
validate_password(_, _) ->
    ?NOT_LOADED.

%%%
%%% Priv
%%%
parse({ok, Line}, File, Acc) ->
    case binary:split(binary:part(Line, 0, byte_size(Line)-1), [<<":">>]) of
	[User, Passwd] ->
	    parse(file:read_line(File), File, gb_trees:insert(User, Passwd, Acc));
	_ ->
	    throw({invalid_db_entry, Line})
    end;
parse(eof, _, Acc) ->
    Acc;
parse({error, Err}, _, _) ->
    throw({invalid_db, Err}).

load() ->
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "htpasswd"), 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
