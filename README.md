# How to run?
```shell
> make -B
> erl -noshell -eval "io_server:start_link(), game:start_link(), spawn(game, play, [])."
```
