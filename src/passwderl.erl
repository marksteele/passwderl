-module(passwderl).
-export([
         getpwnam/1,
         getpwuid/1,
         setuid/1,
         seteuid/1,
         setgid/1,
         setegid/1,
         getuid/0,
         geteuid/0,
         getgid/0,
         getegid/0
        ]).

-compile([no_native]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

-include("passwderl.hrl").


init() ->
  Path = case application:get_env(code,sopath) of
           {ok, CodePath} ->
             CodePath;
           _ ->
             case code:priv_dir(?MODULE) of
               {error, _} ->
                 EbinDir = filename:dirname(code:which(?MODULE)),
                 AppPath = filename:dirname(EbinDir),
                 filename:join(AppPath, "priv");
               CodePath ->
                 CodePath
             end
         end,
  erlang:load_nif(filename:join(Path, ?MODULE), 0).

-spec getpwuid(pos_integer()) -> #passwderl_pwd{}.
getpwuid(Uid) when is_integer(Uid) -> nif_getpwuid(Uid).
-spec getpwnam(list()) -> #passwderl_pwd{}.
getpwnam(Name) when is_list(Name) -> nif_getpwnam(Name).


-spec getuid() -> pos_integer().
getuid() -> nif_getuid().
-spec geteuid() -> pos_integer().
geteuid() -> nif_geteuid().
-spec setuid(pos_integer()) -> ok | error.
setuid(Uid) when is_integer(Uid) -> nif_setuid(Uid).
-spec seteuid(pos_integer()) -> ok | error.
seteuid(Uid) when is_integer(Uid) -> nif_seteuid(Uid).


-spec getgid() -> pos_integer().
getgid() -> nif_getgid().
-spec getegid() -> pos_integer().
getegid() -> nif_getegid().
-spec setgid(pos_integer()) -> ok | error.
setgid(Gid) when is_integer(Gid) -> nif_setgid(Gid).
-spec setegid(pos_integer()) -> ok | error.
setegid(Gid) when is_integer(Gid) -> nif_setegid(Gid).

not_loaded(Line) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_getpwnam(_) -> ?NOT_LOADED.
nif_getpwuid(_) -> ?NOT_LOADED.

nif_seteuid(_) -> ?NOT_LOADED.
nif_setegid(_) -> ?NOT_LOADED.
nif_setuid(_) -> ?NOT_LOADED.
nif_setgid(_) -> ?NOT_LOADED.

nif_getuid() -> ?NOT_LOADED.
nif_geteuid() -> ?NOT_LOADED.
nif_getgid() -> ?NOT_LOADED.
nif_getegid() -> ?NOT_LOADED.
