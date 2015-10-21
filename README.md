# passwderl : An erlang NIF driver to interface Linux passwd functions

API
===

#passwderl_pwd{}
----------------

Lookup functions return a record defined in passwderl.hrl which
contains the following fields:

 - name: A list (string) containing the user name
 - uid: An integer representing the user id
 - gid: An integer representing the user group id
 - home: A list (string) containing the user home directory
 - shell: A list (string) containing the user shell

getpwuid(pos_integer()) -> #passwderl_pwd{}
--------------------------------------------

Takes as an argument an integer of a user id, and returns a record
or throws a bad argument exception.

getpwnam(list()) -> #passwderl_pwd{}
-------------------------------------

Takes as an argument a username string, and returns a record
or throws a bad argument exception.

seteuid(pos_integer()) -> ok | error.
-------------------------------------

Takes as an argument an integer of a user id, and attempts
to set the effective user id to the provided value.

setegid(pos_integer()) -> ok | error.
-------------------------------------

Takes as an argument an integer of a group id, and attempts
to set the effective group id to the provided value.
