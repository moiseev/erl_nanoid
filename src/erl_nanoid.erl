-module(erl_nanoid).

-export([generate/0]).

-export_type([iterator_t/0]).
-export([iterator/0, next/1]).

-ifdef(TEST).
-export([bin_foldl/3]).
-export([pool_size/1]).
-endif.

-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(SIZE, 21).
-define(MASK, 63).

-define(POOL_SIZE, 128).

-doc """
Generates a Nano ID using the default alphabet and size.

Returns a 21-character URL-safe unique ID with ~126 bits of entropy
(similar to UUID v4). Uses cryptographically strong random number
generation via `crypto:strong_rand_bytes/1`.

The default alphabet contains 64 characters: A-Z, a-z, 0-9, `_`, and `-`.

## Returns

A binary containing the generated ID.
""".
-spec generate() -> binary().
generate() ->
    Randomness = crypto:strong_rand_bytes(?SIZE),
    from_randomness(Randomness).

-opaque iterator_t() :: #{pool => binary()}.

-doc """
Creates an iterator with a pre-generated randomness pool.

Use this function when generating multiple IDs for better performance.
The iterator maintains a pool of random bytes that can be used to
generate multiple IDs without repeatedly calling the crypto module.

The iterator should be used with `next/1` to generate IDs.

## Returns

An opaque iterator object.

## See Also

`next/1`
""".
-spec iterator() -> iterator_t().
iterator() ->
    Pool = crypto:strong_rand_bytes(?SIZE * ?POOL_SIZE),
    #{pool => Pool}.

-doc """
Generates the next Nano ID from an iterator.

Takes an iterator (created by `iterator/0` or returned from a
previous call to this function) and returns a tuple containing the
generated ID and a new iterator state.

The iterator automatically replenishes its randomness pool when exhausted,
making it safe to use indefinitely.

## Example

```erlang
It = erl_nanoid:iterator(),
{Id1, It1} = erl_nanoid:next(It),
{Id2, It2} = erl_nanoid:next(It1).
```

## Parameters

- `Iterator`: The iterator object.

## Returns

A tuple `{Id, NewIterator}` where `Id` is the generated binary
and `NewIterator` is the updated iterator state.

## See Also

`iterator/0`
""".
-spec next(iterator_t()) -> {binary(), iterator_t()}.
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

-ifdef(TEST).
pool_size(#{pool := Pool} = _Iterator) when is_binary(Pool) ->
    erlang:byte_size(Pool).
-endif.
