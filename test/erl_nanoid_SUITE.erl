-module(erl_nanoid_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    flat_distribution/1,
    generates_unique_ids/1,
    correct_length/1
]).

%% Test configuration
-define(ALPHABET, <<"useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict">>).
-define(ID_LENGTH, 21).
-define(SAMPLE_COUNT, 100000).
-define(MAX_DISTRIBUTION_DEVIATION, 0.05).

all() ->
    [
        {group, properties}
    ].

groups() ->
    [
        {properties, [parallel], [
            correct_length,
            generates_unique_ids,
            flat_distribution
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
%% Helper functions
%%====================================================================

%% @doc Generate N IDs and build a character frequency histogram
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
