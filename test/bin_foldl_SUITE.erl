-module(bin_foldl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    prop_equivalence_with_lists_foldl/1,
    prop_empty_binary/1,
    prop_single_byte/1,
    prop_associativity/1
]).

all() ->
    [
        {group, properties}
    ].

groups() ->
    [
        {properties, [], [
            prop_equivalence_with_lists_foldl,
            prop_empty_binary,
            prop_single_byte,
            prop_associativity
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Property-based tests
%%====================================================================

%% @doc Main property: bin_foldl should be equivalent to lists:foldl
%% over the binary converted to a list
prop_equivalence_with_lists_foldl(_Config) ->
    Property = ?FORALL(
        {Bin, {FoldFun, Acc}},
        {binary(), fold_function_and_accumulator()},
        begin
            Expected = lists:foldl(FoldFun, Acc, binary_to_list(Bin)),
            Actual = erl_nanoid:bin_foldl(FoldFun, Acc, Bin),
            ?WHENFAIL(
                ct:pal(
                    "Equivalence failed:~n"
                    "  Binary: ~p~n"
                    "  Binary (as list): ~p~n"
                    "  Initial Acc: ~p~n"
                    "  Expected: ~p~n"
                    "  Actual: ~p~n",
                    [Bin, binary_to_list(Bin), Acc, Expected, Actual]
                ),
                Expected =:= Actual
            )
        end
    ),
    ct_property_test:quickcheck(Property, [{property_test_tool, proper}]).

%% @doc Property: folding over empty binary should return the accumulator
prop_empty_binary(_Config) ->
    Property = ?FORALL(
        {FoldFun, Acc},
        fold_function_and_accumulator(),
        begin
            Result = erl_nanoid:bin_foldl(FoldFun, Acc, <<>>),
            ?WHENFAIL(
                ct:pal(
                    "Empty binary property failed:~n"
                    "  Initial Acc: ~p~n"
                    "  Result: ~p~n",
                    [Acc, Result]
                ),
                Result =:= Acc
            )
        end
    ),
    ct_property_test:quickcheck(Property, [{property_test_tool, proper}]).

%% @doc Property: folding over a single byte should apply the function once
prop_single_byte(_Config) ->
    Property = ?FORALL(
        {Byte, {FoldFun, Acc}},
        {byte(), fold_function_and_accumulator()},
        begin
            Bin = <<Byte>>,
            Expected = FoldFun(Byte, Acc),
            Actual = erl_nanoid:bin_foldl(FoldFun, Acc, Bin),
            ?WHENFAIL(
                ct:pal(
                    "Single byte property failed:~n"
                    "  Byte: ~p~n"
                    "  Initial Acc: ~p~n"
                    "  Expected: ~p~n"
                    "  Actual: ~p~n",
                    [Byte, Acc, Expected, Actual]
                ),
                Expected =:= Actual
            )
        end
    ),
    ct_property_test:quickcheck(Property, [{property_test_tool, proper}]).

%% @doc Property: bin_foldl processes bytes from left to right
%% This tests that the order of operations is preserved
prop_associativity(_Config) ->
    Property = ?FORALL(
        Bin,
        non_empty(binary()),
        begin
            %% Collect bytes in order
            CollectFun = fun(Byte, Acc) -> [Byte | Acc] end,
            Result = erl_nanoid:bin_foldl(CollectFun, [], Bin),
            Expected = lists:reverse(binary_to_list(Bin)),
            ?WHENFAIL(
                ct:pal(
                    "Associativity (left-to-right) property failed:~n"
                    "  Binary: ~p~n"
                    "  Binary (as list): ~p~n"
                    "  Expected (reversed): ~p~n"
                    "  Actual: ~p~n",
                    [Bin, binary_to_list(Bin), Expected, Result]
                ),
                Result =:= Expected
            )
        end
    ),
    ct_property_test:quickcheck(Property, [{property_test_tool, proper}]).

%%====================================================================
%% PropEr generators
%%====================================================================

%% @doc Generate a compatible (FoldFun, InitialAcc) pair
fold_function_and_accumulator() ->
    oneof([
        %% Sum all bytes
        {fun(Byte, Acc) when is_integer(Acc) -> Byte + Acc end, 0},
        %% Count bytes
        {fun(_Byte, Acc) when is_integer(Acc) -> Acc + 1 end, 0},
        %% Collect bytes into a list
        {fun(Byte, Acc) when is_list(Acc) -> [Byte | Acc] end, []},
        %% Build a binary (like in generate/0)
        {fun(Byte, Acc) when is_binary(Acc) -> <<Acc/binary, Byte>> end, <<>>},
        %% XOR all bytes
        {fun(Byte, Acc) when is_integer(Acc) -> Byte bxor Acc end, 0},
        %% Keep the max byte seen
        {fun(Byte, Acc) when is_integer(Acc) -> max(Byte, Acc) end, -1},
        %% Keep the min byte seen
        {fun(Byte, Acc) when is_integer(Acc) -> min(Byte, Acc) end, 256},
        %% Multiply with modulo to avoid overflow
        {fun(Byte, Acc) when is_integer(Acc) -> (Byte * Acc) rem 1000000 end, 1}
    ]).
