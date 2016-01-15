#include <stdint.h>
#include "erl_nif.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <pwd.h>
#include <sys/types.h>

static ERL_NIF_TERM create_term(ErlNifEnv *env, struct passwd *pwd)
{
  return enif_make_tuple6(env,
                          enif_make_atom(env, "passwderl_pwd"),
                          enif_make_string(env,(char*) pwd->pw_name, ERL_NIF_LATIN1),
                          enif_make_uint(env,pwd->pw_uid),
                          enif_make_uint(env,pwd->pw_gid),
                          enif_make_string(env,pwd->pw_dir, ERL_NIF_LATIN1),
                          enif_make_string(env,pwd->pw_shell, ERL_NIF_LATIN1)
                          );
}

static ERL_NIF_TERM passwderl_getuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int id = getuid();
  return enif_make_uint(env, id);
}

static ERL_NIF_TERM passwderl_geteuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int id = geteuid();
  return enif_make_uint(env, id);
}

static ERL_NIF_TERM passwderl_getgid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int id = getgid();
  return enif_make_uint(env, id);
}

static ERL_NIF_TERM passwderl_getegid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int id = getegid();
  return enif_make_uint(env, id);
}

static ERL_NIF_TERM passwderl_seteuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int uid;
  if (!enif_get_uint(env,argv[0], &uid)) {
    return enif_make_badarg(env);
  }
  if (seteuid(uid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}

static ERL_NIF_TERM passwderl_setuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int uid;
  if (!enif_get_uint(env,argv[0], &uid)) {
    return enif_make_badarg(env);
  }
  if (setuid(uid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}

static ERL_NIF_TERM passwderl_setegid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int gid;
  if (!enif_get_uint(env,argv[0], &gid)) {
    return enif_make_badarg(env);
  }
  if (setegid(gid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}


static ERL_NIF_TERM passwderl_setgid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int gid;
  if (!enif_get_uint(env,argv[0], &gid)) {
    return enif_make_badarg(env);
  }
  if (setgid(gid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}

static ERL_NIF_TERM passwderl_getpwuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int uid;
  if (!enif_get_uint(env, argv[0], &uid)) {
    return enif_make_badarg(env);
  }
  struct passwd *pwd = getpwuid(uid);

  if (!pwd) {
    return enif_make_badarg(env);
  }
  return create_term(env, pwd);
}

static ERL_NIF_TERM passwderl_getpwnam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int length;
  if (!enif_get_list_length(env, argv[0], &length)) {
    return enif_make_badarg(env);
  }
  char name[length+1];
  if(!enif_get_string(env, argv[0], name,length+1,ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }
  struct passwd *pwd = getpwnam(name);

  if (pwd == NULL) {
    return enif_make_badarg(env);
  }
  return create_term(env, pwd);
}

static ErlNifFunc nif_funcs[] =
{
  {"nif_getpwnam", 1, passwderl_getpwnam},
  {"nif_getpwuid", 1, passwderl_getpwuid},
  {"nif_setuid", 1, passwderl_setuid},
  {"nif_setgid", 1, passwderl_setgid},
  {"nif_seteuid", 1, passwderl_seteuid},
  {"nif_setegid", 1, passwderl_setegid},
  {"nif_getuid", 0, passwderl_getuid},
  {"nif_geteuid", 0, passwderl_geteuid},
  {"nif_getgid", 0, passwderl_getgid},
  {"nif_getegid", 0, passwderl_getegid}
};

ERL_NIF_INIT(passwderl, nif_funcs, NULL, NULL, NULL, NULL)
