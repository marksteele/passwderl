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
                          enif_make_int(env,pwd->pw_uid),
                          enif_make_int(env,pwd->pw_gid),
                          enif_make_string(env,pwd->pw_dir, ERL_NIF_LATIN1),
                          enif_make_string(env,pwd->pw_shell, ERL_NIF_LATIN1)
                          );
}

static ERL_NIF_TERM
passwderl_getpwuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int uid;
  if (!enif_get_int(env, argv[0], &uid)) {
    return enif_make_badarg(env);
  }
  struct passwd *pwd = getpwuid(uid);

  if (!pwd) {
    return enif_make_badarg(env);
  }
  return create_term(env, pwd);
}

static ERL_NIF_TERM
passwderl_getpwnam(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

static ERL_NIF_TERM
passwderl_seteuid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int uid;
  if (!enif_get_int(env,argv[0], &uid)) {
    return enif_make_badarg(env);
  }
  if (seteuid(uid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}

static ERL_NIF_TERM
passwderl_setegid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int gid;
  if (!enif_get_int(env,argv[0], &gid)) {
    return enif_make_badarg(env);
  }
  if (setegid(gid) != 0) {
    return enif_make_atom(env,"error");
  } else {
    return enif_make_atom(env,"ok");
  }
}

static ErlNifFunc nif_funcs[] =
{
  {"nif_getpwnam", 1, passwderl_getpwnam},
  {"nif_getpwuid", 1, passwderl_getpwuid},
  {"nif_seteuid", 1, passwderl_seteuid},
  {"nif_setegid", 1, passwderl_setegid}
};

ERL_NIF_INIT(passwderl, nif_funcs, NULL, NULL, NULL, NULL)
