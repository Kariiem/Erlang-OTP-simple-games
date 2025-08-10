-module(raylib).
-include("raylib.hrl").
-define(NIF_FUNCS, [init_window/3,
                    close_window/0,
                    window_should_close/0,
                    get_frame_time/0,
                    set_target_fps/1,
                    begin_drawing/0,
                    end_drawing/0,
                    clear_background/1,
                    draw_rectangle/5,
                    get_key_pressed/0
                   ]).
-export(?NIF_FUNCS).
-nifs(?NIF_FUNCS).

-export([quicktest/0]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("./raylib_nif", 0).

init_window(_width, _length, _title) ->
    erlang:nif_error(nif_library_not_loaded).

close_window() ->
    erlang:nif_error(nif_library_not_loaded).

window_should_close() ->
    erlang:nif_error(nif_library_not_loaded).

get_frame_time() ->
    erlang:nif_error(nif_library_not_loaded).

set_target_fps(_fps) ->
    erlang:nif_error(nif_library_not_loaded).

begin_drawing() ->
    erlang:nif_error(nif_library_not_loaded).

end_drawing() ->
    erlang:nif_error(nif_library_not_loaded).

clear_background(_c) ->
    erlang:nif_error(nif_library_not_loaded).

draw_rectangle(_x, _y, _w, _h, _c) ->
    erlang:nif_error(nif_library_not_loaded).

get_key_pressed() ->
    erlang:nif_error(nif_library_not_loaded).

%% game_loop(State) ->
%%     case {window_should_close(), State} of
%%         {false, cont} ->
%%             NextState = case get_key_pressed() of
%%                             ?KEY_Q -> quit;
%%                             ?KEY_W -> quit;
%%                             _      -> cont
%%                         end,
%%             begin_drawing(),
%%             clear_background(?BLACK),
%%             end_drawing(),
%%             game_loop(NextState);
%%         _ ->
%%             ok
%%     end.

%% run() ->
%%     init_window(800, 600, "Erlang + Raylib"),
%%     set_target_fps(60),
%%     game_loop(cont),
%%     close_window().

quicktest() ->
    init_window(300, 200, "Quick Test"),
    begin_drawing(),
    clear_background(16#FF0000FF),
    draw_rectangle(50, 50, 100, 50, 16#FF00FF00),
    end_drawing(),
    timer:sleep(100),
    close_window().
