#!/usr/bin/env escript

% Make sure to set the ERL_LIBS properly,
% or just invoke it using `just bench`.

-mode(compile).

main(_) ->
    Report = erlperf:benchmark(
        [
            #{
                label => "generate",
                runner => {erl_nanoid, generate, []}
            },
            #{
                label => "iterator",
                init_runner => {erl_nanoid, iterator, []},
                runner => fun(_Init, It) ->
                    {_Id, It1} = erl_nanoid:next(It),
                    It1
                end
            }
        ],
        #{report => full},
        undefined
    ),
    Out = erlperf_cli:format(Report, #{format => extended, viewport_width => 100}),
    io:format(Out),
    halt(0).
