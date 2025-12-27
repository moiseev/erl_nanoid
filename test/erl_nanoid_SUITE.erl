-module(erl_nanoid_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    flat_distribution/1,
    generates_unique_ids/1,
    correct_length/1
]).
-export([
    iterator_creates_valid_structure/1,
    next_generates_valid_id/1,
    consecutive_calls_produce_different_ids/1,
    pool_gets_consumed/1,
    pool_replenishes_when_exhausted/1,
    many_consecutive_generations/1,
    same_iterator_produces_unique_ids/1,
    different_iterators_produce_unique_ids/1,
    iterator_and_generate_equivalence/1,
    flat_distribution_iterator/1
]).

%% Test configuration
-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(ID_LENGTH, 21).
-define(SAMPLE_COUNT, 100000).
-define(MAX_DISTRIBUTION_DEVIATION, 0.05).

all() ->
    [
        {group, properties},
        {group, iterator_basic},
        {group, iterator_state},
        {group, iterator_uniqueness}
    ].

groups() ->
    [
        {properties, [parallel], [
            correct_length,
            generates_unique_ids,
            flat_distribution
        ]},
        {iterator_basic, [parallel], [
            iterator_creates_valid_structure,
            next_generates_valid_id,
            consecutive_calls_produce_different_ids,
            flat_distribution_iterator
        ]},
        {iterator_state, [], [
            pool_gets_consumed,
            pool_replenishes_when_exhausted,
            many_consecutive_generations
        ]},
        {iterator_uniqueness, [], [
            same_iterator_produces_unique_ids,
            different_iterators_produce_unique_ids,
            iterator_and_generate_equivalence
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test cases
%%====================================================================

%% @doc Test that generated IDs have the correct length
correct_length(_Config) ->
    Id = erl_nanoid:generate(),
    ?assertEqual(?ID_LENGTH, byte_size(Id)).

%% @doc Test that generated IDs are unique
generates_unique_ids(_Config) ->
    Count = 10000,
    Ids = [erl_nanoid:generate() || _ <- lists:seq(1, Count)],
    UniqueIds = sets:from_list(Ids),
    ?assertEqual(Count, sets:size(UniqueIds)).

%% @doc Test that the character distribution is flat (uniform)
%% This validates that the random number generator and the masking
%% approach produce a uniform distribution of characters, which is
%% critical for maintaining the full entropy of the generated IDs.
flat_distribution(_Config) ->
    ct:pal("Generating ~p IDs to test character distribution...", [?SAMPLE_COUNT]),

    %% Generate sample IDs and build character histogram
    Histogram = build_character_histogram(?SAMPLE_COUNT),

    %% Verify all alphabet characters appear
    AlphabetSize = byte_size(?ALPHABET),
    ?assertEqual(
        AlphabetSize,
        maps:size(Histogram),
        "Not all alphabet characters appeared in the sample"
    ),

    %% Calculate normalized distribution for each character
    %% Formula: (char_count * alphabet_size) / (sample_count * id_length)
    %% Perfect uniform distribution = 1.0
    Distributions = maps:map(
        fun(_Char, Count) ->
            (Count * AlphabetSize) / (?SAMPLE_COUNT * ?ID_LENGTH)
        end,
        Histogram
    ),

    %% Find max and min distributions
    DistValues = maps:values(Distributions),
    Max = lists:max(DistValues),
    Min = lists:min(DistValues),
    Deviation = Max - Min,

    ct:pal(
        "Distribution stats:~n"
        "  Min: ~.4f~n"
        "  Max: ~.4f~n"
        "  Deviation: ~.4f~n"
        "  Allowed: ~.4f",
        [Min, Max, Deviation, ?MAX_DISTRIBUTION_DEVIATION]
    ),

    %% Assert the deviation is within acceptable bounds
    ?assert(
        Deviation =< ?MAX_DISTRIBUTION_DEVIATION,
        lists:flatten(
            io_lib:format(
                "Character distribution deviation (~.4f) exceeds threshold (~.4f)",
                [Deviation, ?MAX_DISTRIBUTION_DEVIATION]
            )
        )
    ).

%%====================================================================
%% Iterator tests
%%====================================================================

%% Test that iterator creates a valid structure
iterator_creates_valid_structure(_Config) ->
    It = erl_nanoid:iterator(),
    PoolSize = erl_nanoid:pool_size(It),
    ?assert(PoolSize > 0).

%% Test that next/1 generates valid IDs
next_generates_valid_id(_Config) ->
    It = erl_nanoid:iterator(),
    {Id, _It1} = erl_nanoid:next(It),
    ?assertEqual(?ID_LENGTH, byte_size(Id)),
    %% Verify all characters are from the alphabet
    ?assert(
        lists:all(
            fun(Char) ->
                binary:match(?ALPHABET, <<Char>>) =/= nomatch
            end,
            binary:bin_to_list(Id)
        )
    ).

%% Test that consecutive next/1 calls produce different IDs
consecutive_calls_produce_different_ids(_Config) ->
    It = erl_nanoid:iterator(),
    {Id1, It1} = erl_nanoid:next(It),
    {Id2, It2} = erl_nanoid:next(It1),
    {Id3, _It3} = erl_nanoid:next(It2),
    ?assertNotEqual(Id1, Id2),
    ?assertNotEqual(Id2, Id3),
    ?assertNotEqual(Id1, Id3).

%% Test that pool gets consumed
pool_gets_consumed(_Config) ->
    It = erl_nanoid:iterator(),
    PoolSize1 = erl_nanoid:pool_size(It),
    {_Id, It1} = erl_nanoid:next(It),
    PoolSize2 = erl_nanoid:pool_size(It1),
    %% Pool should be 21 bytes smaller
    ?assertEqual(PoolSize1 - ?ID_LENGTH, PoolSize2).

%% Test that pool replenishes when exhausted
pool_replenishes_when_exhausted(_Config) ->
    It = erl_nanoid:iterator(),
    InitialSize = erl_nanoid:pool_size(It),
    GenerationCount = InitialSize div ?ID_LENGTH,

    %% Drain the pool
    ItDrained = lists:foldl(
        fun(_, ItAcc) ->
            {_Id, ItNew} = erl_nanoid:next(ItAcc),
            ItNew
        end,
        It,
        lists:seq(1, GenerationCount)
    ),

    %% Next call should replenish
    {_Id, ItReplenished} = erl_nanoid:next(ItDrained),
    NewPoolSize = erl_nanoid:pool_size(ItReplenished),
    %% Pool should be replenished (close to initial size minus one ID)
    ?assert(NewPoolSize > ?ID_LENGTH).

%% Test many consecutive generations
many_consecutive_generations(_Config) ->
    Count = 1000,
    It = erl_nanoid:iterator(),
    {Ids, _FinalIt} = lists:foldl(
        fun(_, {IdsAcc, ItAcc}) ->
            {Id, ItNew} = erl_nanoid:next(ItAcc),
            {[Id | IdsAcc], ItNew}
        end,
        {[], It},
        lists:seq(1, Count)
    ),
    %% All should be unique
    UniqueIds = sets:from_list(Ids),
    ?assertEqual(Count, sets:size(UniqueIds)),
    %% All should have correct length
    ?assert(
        lists:all(
            fun(Id) -> byte_size(Id) =:= ?ID_LENGTH end,
            Ids
        )
    ).

%% Test IDs from same iterator are unique
same_iterator_produces_unique_ids(_Config) ->
    Count = 10000,
    It = erl_nanoid:iterator(),
    Ids = generate_n_ids_from_iterator(Count, It),
    UniqueIds = sets:from_list(Ids),
    ?assertEqual(Count, sets:size(UniqueIds)).

%% Test IDs from different iterators are unique
different_iterators_produce_unique_ids(_Config) ->
    Count = 1000,
    It1 = erl_nanoid:iterator(),
    It2 = erl_nanoid:iterator(),

    Ids1 = generate_n_ids_from_iterator(Count, It1),
    Ids2 = generate_n_ids_from_iterator(Count, It2),

    AllIds = Ids1 ++ Ids2,
    UniqueIds = sets:from_list(AllIds),
    %% Should have 2*Count unique IDs
    ?assertEqual(Count * 2, sets:size(UniqueIds)).

%% Test that iterator and generate produce equivalent IDs
iterator_and_generate_equivalence(_Config) ->
    Count = 100,

    %% Generate IDs using both methods
    It = erl_nanoid:iterator(),
    IteratorIds = generate_n_ids_from_iterator(Count, It),
    GenerateIds = [erl_nanoid:generate() || _ <- lists:seq(1, Count)],

    %% Both should produce correct length
    ?assert(lists:all(fun(Id) -> byte_size(Id) =:= ?ID_LENGTH end, IteratorIds)),
    ?assert(lists:all(fun(Id) -> byte_size(Id) =:= ?ID_LENGTH end, GenerateIds)),

    %% Both should use valid characters
    ValidChars = fun(Id) ->
        lists:all(
            fun(Char) -> binary:match(?ALPHABET, <<Char>>) =/= nomatch end,
            binary:bin_to_list(Id)
        )
    end,
    ?assert(lists:all(ValidChars, IteratorIds)),
    ?assert(lists:all(ValidChars, GenerateIds)).

%% Test that the character distribution from iterator is flat (uniform)
%% This validates that the iterator produces the same quality of randomness
%% as the generate/0 function, maintaining full entropy of the generated IDs.
flat_distribution_iterator(_Config) ->
    ct:pal("Generating ~p IDs using iterator to test character distribution...", [?SAMPLE_COUNT]),

    %% Generate sample IDs using iterator and build character histogram
    Histogram = build_character_histogram_from_iterator(?SAMPLE_COUNT),

    %% Verify all alphabet characters appear
    AlphabetSize = byte_size(?ALPHABET),
    ?assertEqual(
        AlphabetSize,
        maps:size(Histogram),
        "Not all alphabet characters appeared in the sample"
    ),

    %% Calculate normalized distribution for each character
    %% Formula: (char_count * alphabet_size) / (sample_count * id_length)
    %% Perfect uniform distribution = 1.0
    Distributions = maps:map(
        fun(_Char, Count) ->
            (Count * AlphabetSize) / (?SAMPLE_COUNT * ?ID_LENGTH)
        end,
        Histogram
    ),

    %% Find max and min distributions
    DistValues = maps:values(Distributions),
    Max = lists:max(DistValues),
    Min = lists:min(DistValues),
    Deviation = Max - Min,

    ct:pal(
        "Distribution stats (iterator):~n"
        "  Min: ~.4f~n"
        "  Max: ~.4f~n"
        "  Deviation: ~.4f~n"
        "  Allowed: ~.4f",
        [Min, Max, Deviation, ?MAX_DISTRIBUTION_DEVIATION]
    ),

    %% Assert the deviation is within acceptable bounds
    ?assert(
        Deviation =< ?MAX_DISTRIBUTION_DEVIATION,
        lists:flatten(
            io_lib:format(
                "Character distribution deviation (~.4f) exceeds threshold (~.4f)",
                [Deviation, ?MAX_DISTRIBUTION_DEVIATION]
            )
        )
    ).

%%====================================================================
%% Helper functions
%%====================================================================

%% Helper function to generate N IDs from an iterator
generate_n_ids_from_iterator(N, Iterator) ->
    {Ids, _} = lists:foldl(
        fun(_, {IdsAcc, ItAcc}) ->
            {Id, ItNew} = erl_nanoid:next(ItAcc),
            {[Id | IdsAcc], ItNew}
        end,
        {[], Iterator},
        lists:seq(1, N)
    ),
    Ids.

%% Generate N IDs using iterator and build a character frequency histogram
build_character_histogram_from_iterator(Count) ->
    It = erl_nanoid:iterator(),
    {Histogram, _} = lists:foldl(
        fun(_, {HistAcc, ItAcc}) ->
            {Id, ItNew} = erl_nanoid:next(ItAcc),
            {update_histogram_with_id(Id, HistAcc), ItNew}
        end,
        {#{}, It},
        lists:seq(1, Count)
    ),
    Histogram.

%% Generate N IDs and build a character frequency histogram
-spec build_character_histogram(pos_integer()) -> #{char() => non_neg_integer()}.
build_character_histogram(Count) ->
    lists:foldl(
        fun(_I, Histogram) ->
            Id = erl_nanoid:generate(),
            update_histogram_with_id(Id, Histogram)
        end,
        #{},
        lists:seq(1, Count)
    ).

%% @doc Update histogram with character frequencies from an ID
-spec update_histogram_with_id(binary(), #{char() => non_neg_integer()}) ->
    #{char() => non_neg_integer()}.
update_histogram_with_id(Id, Histogram) ->
    lists:foldl(
        fun(Char, Acc) ->
            maps:update_with(Char, fun(V) -> V + 1 end, 1, Acc)
        end,
        Histogram,
        binary:bin_to_list(Id)
    ).
