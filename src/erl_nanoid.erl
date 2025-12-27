-module(erl_nanoid).

-export([naive/0]).

-export([iterator/0, next/1]).

-ifdef(TEST).
-export([bin_foldl/3]).
-endif.

-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(SIZE, 21).
-define(MASK, 63).

-define(POOL_SIZE, 128).

-spec naive() -> binary.
naive() ->
    Randomness = crypto:strong_rand_bytes(?SIZE),
    from_randomness(Randomness).

iterator() ->
    Pool = crypto:strong_rand_bytes(?SIZE * ?POOL_SIZE),
    #{pool => Pool}.

next(#{pool := Pool} = _Iterator) ->
    Pool1 =
        case erlang:byte_size(Pool) < ?SIZE of
            true -> crypto:strong_rand_bytes(?SIZE * ?POOL_SIZE);
            false -> Pool
        end,
    <<Randomness:?SIZE/binary, Rest/binary>> = Pool1,
    Id = from_randomness(Randomness),
    {Id, #{pool => Rest}}.

%% Implementation details

from_randomness(Randomness) when is_binary(Randomness) ->
    bin_foldl(
        fun(B, Acc) ->
            Ch = binary:at(?ALPHABET, B band ?MASK),
            <<Acc/binary, Ch>>
        end,
        <<>>,
        Randomness
    ).

bin_foldl(F, Acc, Bin) when is_binary(Bin) andalso is_function(F, 2) ->
    bin_foldl_impl(F, Acc, Bin).

bin_foldl_impl(_, Acc, <<>>) ->
    Acc;
bin_foldl_impl(F, Acc, <<Head, Tail/binary>>) ->
    bin_foldl_impl(F, F(Head, Acc), Tail).
