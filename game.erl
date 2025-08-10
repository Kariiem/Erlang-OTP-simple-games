%% game.erl
%% this file implements the game server
-module(game).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([guess/1, new_game/0]).
-export([play/0]).

start_link() ->
    DEBUG = [{debug, [trace, {log_to_file, "debug.log"}]}],
    gen_server:start_link({local, game}, game, [], []).

init(_Arg) ->
    %% Target = rand:uniform(100),  % Random number 1-100
    Target = 10,  % Random number 1-100
    State = #{target => Target, guesses => 0},
    {ok, State}.

handle_call(new_game, _From, _State) ->
    Target = rand:uniform(100),
    NewState = #{target => Target, guesses => 0},
    {reply, ok, NewState};
handle_call({guess, Guess}, _From, State = #{target := Target, guesses := Count}) ->
    NewCount = Count + 1,
    NewState = State#{guesses => NewCount},
    if
        Guess == Target ->
            {reply, {correct, NewCount}, NewState};
        Guess < Target ->
            {reply, {too_low, NewCount}, NewState};
        true ->
            {reply, {too_high, NewCount}, NewState}
    end.

handle_cast(_Req, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play() ->
    Num = io_server:read(),
    case guess(Num) of
        {correct, _} ->
            io_server:write(done),
            io_server:ask();
        {too_low, _} ->
            io_server:write(low),
            play();
        {too_high, _} ->
            io_server:write(high),
            play()
    end.

guess(Guess) ->
    gen_server:call(game, {guess, Guess}).

new_game() ->
    gen_server:call(game, new_game).
