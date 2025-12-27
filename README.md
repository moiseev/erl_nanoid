# erl_nanoid

A secure, URL-friendly unique string ID generator for Erlang.

Nano ID implementation in pure Erlang, inspired by and ported from [NanoID](https://github.com/ai/nanoid).

## Features

- **Compact**: Generates 21-character IDs by default (similar security to UUID v4 with 126 bits of entropy)
- **URL-safe**: Uses a safe alphabet without special characters
- **Secure**: Uses cryptographically strong random number generation via `crypto:strong_rand_bytes/1`
- **Zero dependencies**: Pure Erlang implementation with no external dependencies
- **Straightforward**: Simple implementation

## Requirements

- Erlang/OTP 21 or higher

## Installation

Add `erl_nanoid` as a dependency in your `rebar.config`:

```erlang
{deps, [
    {erl_nanoid, {git, "https://github.com/moiseev/erl_nanoid.git", {branch, "main"}}}
]}.
```

Then compile:

```bash
rebar3 compile
```

## Usage

```erlang
1> erl_nanoid:generate().
<<"TAHpcYCYdmrdPYlxxKs1-">>
2> erl_nanoid:generate().
<<"3-q21YGrFjxa4u6ftT_Sd">>
```

or

```erlang
1> It = erl_nanoid:iterator().
#{pool =>
      <<193,229,141,138,225,104,40,210,183,103,239,44,211,52,
        10,49,150,222,18,81,255,47,219,145,158,142,184,84,...>>}
2> {Id0, It0} = erl_nanoid:next(It).
{<<"sU96IWWPqHZ_Xj6fpRP0t">>,
 #{pool =>
       <<47,219,145,158,142,184,84,134,89,175,76,123,218,245,
         45,22,105,163,112,68,181,126,87,126,184,79,...>>}}
3> {Id1, It1} = erl_nanoid:next(It0).
{<<"ZK0R8v7oAZ1zCkGpODbnk">>,
 #{pool =>
       <<126,87,126,184,79,222,242,7,116,87,251,79,172,33,31,
         112,52,68,37,113,208,81,138,226,63,255,...>>}}
```

### In Your Application

```erlang
-module(my_app).
-export([create_user/1]).

create_user(Name) ->
    UserId = erl_nanoid:generate(),
    %% Use the generated ID
    {ok, #{id => UserId, name => Name}}.
```

## API

### `generate/0`

Generates a Nano ID using the default alphabet and size.

- **Returns**: A binary containing a 21-character URL-safe unique ID
- **Alphabet**: `useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict` (64 characters: A-Z, a-z, 0-9, `_`, `-`)
- **Size**: 21 characters
- **Entropy**: ~126 bits (similar to UUID v4)

```erlang
-spec generate() -> binary().
```

### `iterator/0`

Creates an iterator with a pre-generated randomness pool for efficient batch ID generation.

- **Returns**: An iterator object.
- **Use case**: When generating multiple IDs, this approach is more efficient than calling `generate/0` repeatedly

```erlang
-spec iterator() -> iterator().
```

### `next/1`

Generates the next Nano ID from an iterator.

- **Parameter**: Iterator object (as returned by `iterator/0` or a previous `next/1` call)
- **Returns**: A tuple `{Id, NewIterator}` where `Id` is the generated binary and `NewIterator` is the updated iterator state

```erlang
-spec next(iterator()) -> {binary(), iterator()}.
```

## Performance

```
$ just bench
Code         ||   Samples       Avg   StdDev    Median      P99  Iteration    Rel
iterator      1         3   2052 Ki    0.61%   2058 Ki  2061 Ki     487 ns   100%
generate      1         3   1325 Ki    0.73%   1330 Ki  1331 Ki     754 ns    65%
```

## Development

### Building

```bash
rebar3 compile
```

### Running in Shell

```bash
rebar3 shell
```

### Formatting

This project uses [erlfmt](https://github.com/WhatsApp/erlfmt) for code formatting:

```bash
rebar3 fmt
```

### Git Hooks

To automatically check formatting before commits, install [pre-commit](https://pre-commit.com/):

```bash
# Install pre-commit (one-time setup)
brew install pre-commit  # macOS
# or: pip install pre-commit

# Install the git hooks (run in project directory)
pre-commit install
```

The hook will run `rebar3 fmt --check` before each commit.

## Comparison with Other Implementations

For a more full-featured Elixir implementation with additional options (custom alphabets, custom sizes), see [nanoid](https://hex.pm/packages/nanoid).

This Erlang implementation focuses on simplicity and zero dependencies, providing the core functionality of generating secure unique IDs.

## License

See [LICENSE.md](LICENSE.md) for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Resources

- [NanoID Official Site](https://github.com/ai/nanoid)
- [NanoID Collision Calculator](https://zelark.github.io/nano-id-cc/)
