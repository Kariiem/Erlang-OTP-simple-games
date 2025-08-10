-module(io_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([read/0, ask/0, write/1]).

start_link() ->
    DEBUG = [{debug, [trace]}],
    gen_server:start_link({local, io_server},
                          io_server, [], DEBUG).

init(_) ->
    {ok, File} = file:open("game_output.log", [write]),
    {ok, #{output_file => File}}.

handle_call(read_number, _From, #{output_file := File} = State) ->
    case io:get_line("Number: ") of
        eof -> error;
        Line ->
            Trimmed = string:trim(Line),
            try
                {reply, list_to_integer(Trimmed), State}
            catch
                _:_ ->
                    io:format(File, "Invalid number~n"),
                    read()
            end
    end;

handle_call(ask, From, #{output_file := File} = State) ->
    case io:fread("(y/n): ", "~s") of
        {ok, ["y"]} ->
            {reply, yes, State};
        _ ->
            io:format(File, "Thanks [Player ~p] for playing!~n", [From]),
            {reply, no, State}
    end.

handle_cast({low, PID}, #{output_file := File} = State) ->
    io:format(File, "[Player ~p] Too low!~n", [PID]),
    {noreply, State};
handle_cast({high, PID}, #{output_file := File} = State) ->
    io:format(File, "[Player ~p] Too high!~n", [PID]),
    {noreply, State};
handle_cast({done, PID}, #{output_file := File} = State) ->
    io:format(File, "[Player ~p] You won!~n", [PID]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read() ->
    gen_server:call(io_server, read_number).

write(low) ->
    gen_server:cast(io_server, {low, self()});
write(high) ->
    gen_server:cast(io_server, {high, self()});
write(done) ->
    gen_server:cast(io_server, {done, self()}).

ask() ->
    case gen_server:call(io_server, ask) of
        yes ->
            game:play();
        no -> ok
    end.
