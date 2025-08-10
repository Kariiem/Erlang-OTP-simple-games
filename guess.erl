%% guess.erl
%% this file implements the client/playing entity
-module(guess).
-export([run/0, play/0]).

run() ->
    game:start_link(),
    play().


play() ->
    case io:fread("Guess a number (1-100): ", "~d") of
        {ok, [Num]} ->
            case game:guess(Num) of
                {correct, Count} ->
                    io:format("You won in ~p guesses!~n", [Count]),
                    ask_play_again();
                {too_low, _} ->
                    io:format("client: Too low!~n"),
                    play();
                {too_high, _} ->
                    io:format("client: Too high!~n"),
                    play()
            end;
        {error, _} ->
            io:format("Please enter a valid number~n"),
            play()
    end.

ask_play_again() ->
    case io:fread("Play again? (y/n): ", "~s") of
        {ok, ["y"]} ->
            game:new_game(),
            play();
        _ ->
            io:format("Thanks for playing!~n")
    end.
