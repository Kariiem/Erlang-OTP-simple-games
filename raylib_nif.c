#include <stdint.h>

#include <raylib.h>
#include <erl_nif.h>

#define CONCAT_(a, b) a ## b
#define CONCAT(a, b) CONCAT(a, b)

#define nif_register(fun, arity) {#fun, arity, CONCAT(fun, nif)}

static ERL_NIF_TERM
init_window_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int width;
    int height;
    char title[256];

    if (!enif_get_int(env, argv[0], &width)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &height)) return enif_make_badarg(env);
    if (!enif_get_string(env, argv[2], title, sizeof(title), ERL_NIF_LATIN1)) return enif_make_badarg(env);

    InitWindow(width, height, title);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
close_window_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CloseWindow();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
window_should_close_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = WindowShouldClose();
    return enif_make_atom(env, !!(ret) ? "true" : "false");
}

static ERL_NIF_TERM
set_target_fps_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fps = 0;
    if (!enif_get_int(env, argv[0], &fps)) return enif_make_badarg(env);
    SetTargetFPS(fps);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
begin_drawing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    BeginDrawing();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
end_drawing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    EndDrawing();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
clear_background_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Color rcolor;
    uint64_t color;

    if (!enif_get_uint64(env, argv[0], &color)) return enif_make_badarg(env);

    rcolor.a = (color >> (8 * 3)) & 0xFF;
    rcolor.r = (color >> (8 * 2)) & 0xFF;
    rcolor.g = (color >> (8 * 1)) & 0xFF;
    rcolor.b = (color >> (8 * 0)) & 0xFF;

    ClearBackground(rcolor);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
draw_rectangle_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int posX;
    int posY;
    int width;
    int height;
    Color rcolor;
    uint64_t color;

    if (!enif_get_int(env, argv[0], &posX))     return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &posY))     return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &width))    return enif_make_badarg(env);
    if (!enif_get_int(env, argv[3], &height))   return enif_make_badarg(env);
    if (!enif_get_uint64(env, argv[4], &color)) return enif_make_badarg(env);

    rcolor.a = (color >> (8 * 3)) & 0xFF;
    rcolor.r = (color >> (8 * 2)) & 0xFF;
    rcolor.g = (color >> (8 * 1)) & 0xFF;
    rcolor.b = (color >> (8 * 0)) & 0xFF;

    DrawRectangle(posX, posY, width, height, rcolor);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
get_key_pressed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret = GetKeyPressed();
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM
get_frame_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double ft = GetFrameTime();
    return enif_make_double(env, ft);
}

static ErlNifFunc nif_funcs[] = {
    {"get_frame_time"      , 0, get_frame_time_nif}      ,
    {"init_window"         , 3, init_window_nif}         ,
    {"close_window"        , 0, close_window_nif}        ,
    {"window_should_close" , 0, window_should_close_nif} ,
    {"set_target_fps"      , 1, set_target_fps_nif}      ,
    {"begin_drawing"       , 0, begin_drawing_nif}       ,
    {"end_drawing"         , 0, end_drawing_nif}         ,
    {"clear_background"    , 1, clear_background_nif}    ,
    {"draw_rectangle"      , 5, draw_rectangle_nif}      ,
    {"get_key_pressed"     , 0, get_key_pressed_nif}
};

// Hot reload support callbacks
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    // Called when the NIF is first loaded
    // You can initialize global state here if needed
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    // Called when the same NIF is loaded again
    // This is where hot reloading happens
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    // Called when a new version of the NIF is loaded
    // Transfer any state from old to new version
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    // Called when the NIF is unloaded
    // Clean up any resources here
    // Note: This gets called during hot reloads too
}


ERL_NIF_INIT(raylib, nif_funcs, NULL, NULL, upgrade, NULL)
