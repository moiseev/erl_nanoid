-module(erl_nanoid).

-export([naive/0]).

-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(SIZE, 21).
-define(MASK, 63).

-spec naive() -> binary.
naive() ->
    Randomness = binary:bin_to_list(crypto:strong_rand_bytes(?SIZE)),
    lists:foldl(
        fun(B, Acc) ->
            Ch = binary:at(?ALPHABET, B band ?MASK),
            <<Acc/binary, Ch>>
        end,
        <<>>,
        Randomness
    ).
