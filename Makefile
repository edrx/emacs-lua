EMACS_DIR ?= "$(HOME)/src/emacs"
LUA_INCLUDE_DIR ?= /usr/include/lua5.3

emlua.so:
	g++ -I$(EMACS_DIR)/src -I$(LUA_INCLUDE_DIR) -shared emlua.cpp -o $@ -llua5.3

clean:
	rm -fv emlua.so

.PHONY: clean
