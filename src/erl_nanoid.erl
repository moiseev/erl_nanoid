-module(erl_nanoid).

-export([naive/0]).

-ifdef(TEST).
-export([bin_foldl/3]).
-endif.

-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(SIZE, 21).
-define(MASK, 63).

-spec naive() -> binary.
naive() ->
    Randomness = crypto:strong_rand_bytes(?SIZE),
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
