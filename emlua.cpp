// This file:
//   http://angg.twu.net/emacs-lua/emlua.cpp.html
//   http://angg.twu.net/emacs-lua/emlua.cpp
//           (find-angg "emacs-lua/emlua.cpp")
// Author: <nerditation@outlook.com>
//    See: http://lua-users.org/lists/lua-l/2021-03/msg00084.html
//         https://lists.gnu.org/archive/html/emacs-devel/2021-04/msg00907.html
// Some comments by: Eduardo Ochs <eduardoochs@gmail.com>
//
// emlua.cpp - a emacs module that runs Lua code.
// Adapted from the code that nerditation sent to lua-l.
// Nerditation's original instructions on how to compile this were just this line:
//   g++ -IZ:/emacs/include -IZ:/Lua/include -shared emlua -o emlua.dll -LZ:/Lua/lib -llua
// My notes on how to compile this on Debian are at the end of this file.
//
// (defun e () (interactive) (find-angg "emacs-lua/emlua.cpp"))
// (defun l () (interactive) (find-angg "emacs-lua/emlua.el"))

#include <vector>
#include <emacs-module.h>
#include <lua.hpp>

int plugin_is_GPL_compatible;

// TODO: convert lua values to elisp values in a meaningful way.
// PLACEHOLDER: call `luaL_tolstring` on everything
static emacs_value lua_to_elisp(lua_State *L, emacs_env *env, int i) {
	size_t size;
	auto s = luaL_tolstring(L, i, &size);
	return env->make_string(env, s, size);
}

#define EMACS_ENV_KEY "*emacs_env"

// ef_xxx is elisp function so uses emacs-module-func protocol
// basically a wrapper around the Lua `dostring` function
// returns a vector containing the multiple (possibly zero) return values (called `tostring` on them) of the Lua code
// returns an error message on failure
static emacs_value ef_lua_dostring(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
	// closure data is lua_State
	lua_State *L = (lua_State *)data;
	// the env is valid on for this callstack
	lua_pushlightuserdata(L, env);
	lua_setfield(L, LUA_REGISTRYINDEX, EMACS_ENV_KEY);
	// string length: emacs uses signed type (ptrdiff_t), Lua uses unsigned type (size_t)
	ptrdiff_t len = 0;
	// emacs didn't provide API to `borrow` the string
	// we are forced to make a copy and then Lua will copy it again
	env->copy_string_contents(env, args[0], nullptr, &len);
	auto buffer = std::vector<char>(len);
	env->copy_string_contents(env, args[0], buffer.data(), &len);
	//assert(buffer.back() == '\0');
	auto status = luaL_dostring(L, buffer.data());
	if (status != LUA_OK) {
		auto ret = lua_to_elisp(L, env, -1);
		lua_settop(L, 0);
		return ret;
	}
	auto multret = std::vector<emacs_value>{};
	int retcount = lua_gettop(L);
	multret.reserve(retcount);
	for (int i = 1; i <= retcount; ++i) {
		multret.push_back(lua_to_elisp(L, env, i));
	}
	lua_settop(L, 0);
	return env->funcall(env, env->intern(env, "vector"), multret.size(), multret.data());
}

// lf_xxx is lua function so use lua_CFunction protocol
static int lf_message(lua_State *L)
{
	lua_getfield(L, LUA_REGISTRYINDEX, EMACS_ENV_KEY);
	auto *env = (emacs_env *)lua_touserdata(L, -1);
	size_t size;
	auto s = luaL_tolstring(L, 1, &size);
	emacs_value args[1] = {env->make_string(env, s, size)};
	env->funcall(env, env->intern(env, "message"), 1, args);
	return 0;
};

extern "C" {
int emacs_module_init(struct emacs_runtime *ert) noexcept
{
	emacs_env *env = ert->get_environment(ert);
	lua_State *L = luaL_newstate();
	luaL_openlibs(L);
	// register Lua callable function(s)
	lua_pushcfunction(L, lf_message);
	lua_setglobal(L, "message");
	// register elisp callable function(s)
	emacs_value func = env->make_function(
			env,
			1, // min_arity,
			1, // max_arity,
			&ef_lua_dostring,
			"run string as Lua code",
			L
			);
	emacs_value symbol = env->intern(env, "emlua-dostring");
	emacs_value args[] = {symbol, func};
	env->funcall(env, env->intern(env, "defalias"), 2, args);
	return 0;
}
} // extern "C"


/*
# (find-es "emacs" "vterm-recompile")
# (find-sh "locate emacs-module.h")
# (find-sh "locate lua.hpp")
# (find-sh "locate liblua5.3")
# (find-fline "~/bigsrc/emacs27/src/" "emacs-module.h")
# (find-fline "/usr/include/lua5.3/" "lua.hpp")
# (find-fline "/usr/lib/x86_64-linux-gnu/" "liblua5.3.so")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

# (find-fline "~/emacs-lua/")
cd ~/emacs-lua/
ls -lAF
g++ -I$HOME/bigsrc/emacs27/src \
    -I/usr/include/lua5.3 \
    -shared \
    emlua.cpp \
    -o emlua.so \
    -llua5.3

cd ~/emacs-lua/
ls -lAF
g++ -I$HOME/bigsrc/emacs28/src \
    -I/usr/include/lua5.3 \
    -shared \
    emlua.cpp \
    -o emlua.so \
    -llua5.3

# (find-fline "~/emacs-lua/")
# (find-fline "~/emacs-lua/" "emlua.so")
# (load       "~/emacs-lua/emlua.so")

(emlua-dostring "return 22")
(emlua-dostring "return 22+33")
(emlua-dostring "return 22+33, 44")
(emlua-dostring "return 22+33, '44', {}")
(emlua-dostring "a = nil")
(emlua-dostring "a = 22")
(emlua-dostring "return a+33, '44', {}")
(emlua-dostring "return a")


 (load "~/emacs-lua/emlua.so")
 (load "~/emacs-lua/emlua.el")

 (eepitch-emlua)
 (eepitch-kill)
 (eepitch-emlua)
return 22
return 22, 30+4, "foo"
Err
a = 22
return a..a


 (emlua-dostring "dofile '/home/edrx/LUA/lua50init.lua'")
 (emlua-dostring "ee_dofile   '~/edrxrepl/edrxrepl.lua'")
 (emlua-dostring "REPL = Repl:new()")
 (emlua-dostring "REPL.esuccessprint = REPL.esuccessprint0")

 (eepitch-emluarepl)
 (eepitch-kill)
 (eepitch-emluarepl)
 (eepitch-emluarepl-prompt)
= 1 + 2
a = 22
= a + 22
FOO!!!
= a + nil
= REPL
= {2,3}
= Repl
= mytabletostring(Repl)

REPL2 = Repl:new()
= REPL2

*/
