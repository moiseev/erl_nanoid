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
1> erl_nanoid:naive().
<<"TAHpcYCYdmrdPYlxxKs1-">>
2> erl_nanoid:naive().
<<"3-q21YGrFjxa4u6ftT_Sd">>
```

### In Your Application

```erlang
-module(my_app).
-export([create_user/1]).

create_user(Name) ->
    UserId = erl_nanoid:naive(),
    %% Use the generated ID
    {ok, #{id => UserId, name => Name}}.
```

## API

### `naive/0`

Generates a Nano ID using the default alphabet and size.

- **Returns**: A binary containing a 21-character URL-safe unique ID
- **Alphabet**: `useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict` (64 characters: A-Z, a-z, 0-9, `_`, `-`)
- **Size**: 21 characters
- **Entropy**: ~126 bits (similar to UUID v4)

```erlang
-spec naive() -> binary().
```

**Note**: This is called "naive" because it uses a simple implementation. Future versions may include optimized implementations with custom alphabets and sizes.

## Performance

```
$ ERL_LIBS=_build/default/lib/ erlperf --samples 3 'erl_nanoid:naive().'
WARNING: Dynamic Trace Probes enabled (dtrace detected)
Code                        ||        QPS       Time
erl_nanoid:naive().          1    1435 Ki     697 ns
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
