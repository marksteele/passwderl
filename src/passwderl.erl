-module(passwderl).
-export([
         getpwnam/1,
         getpwuid/1,
         seteuid/1,
         setegid/1
        ]).

-compile([no_native]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

-include("passwderl.hrl").

init() ->
  PrivDir = case code:priv_dir(?MODULE) of
              {error, _} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
              Path ->
                Path
            end,
  erlang:load_nif(filename:join(PrivDir, "passwderl"), 0).

-spec getpwuid(pos_integer()) -> #passwderl_pwd{}.
getpwuid(Uid) when is_integer(Uid) ->
  nif_getpwuid(Uid).

-spec getpwnam(list()) -> #passwderl_pwd{}.
getpwnam(Name) when is_list(Name) ->
  nif_getpwnam(Name).

-spec seteuid(pos_integer()) -> ok | error.
seteuid(Uid) when is_integer(Uid) ->
  nif_seteuid(Uid).

-spec setegid(pos_integer()) -> ok | error.
setegid(Gid) when is_integer(Gid) ->
  nif_setegid(Gid).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_getpwnam(_) ->
  ?NOT_LOADED.

nif_getpwuid(_) ->
  ?NOT_LOADED.

nif_seteuid(_) ->
  ?NOT_LOADED.

nif_setegid(_) ->
  ?NOT_LOADED.
