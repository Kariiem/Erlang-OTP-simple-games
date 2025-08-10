erl_path = /usr/lib/erlang/usr/include/
raylib_path = raylib-5.5_linux_amd64/include
library_paths = raylib-5.5_linux_amd64/lib

all: game.beam guess.beam io_server.beam

io_server.beam: io_server.erl
	erlc io_server.erl

guess.beam: game.beam guess.erl
	erlc guess.erl

game.beam: raylib.beam game.erl
	erlc game.erl

############################################################
# Raylib nif
############################################################
test: raylib_nif.so raylib.beam
	@echo "Testing raylib application..."
	erl -noshell -eval "raylib:quicktest(), init:stop()."

raylib.beam: raylib.erl raylib_nif.so
	erlc raylib.erl

raylib_nif.so: raylib_nif.c
	gcc -I$(erl_path) -I$(raylib_path) -shared raylib_nif.c -L$(library_paths) -lraylib -o raylib_nif.so
############################################################

.PHONY: clean
clean:
	rm -f raylib_nif.so *.beam *.dump


run: raylib_nif.so raylib.beam
	@echo "Starting raylib application..."
	erl -noshell -eval "raylib:run(), init:stop()."
