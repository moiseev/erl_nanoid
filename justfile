setup:
    @echo "Installing git hooks..."
    pre-commit install
    @echo "Setup complete!"

compile:
    rebar3 compile

check-binaries:
    rebar3 as bin_opt_info compile 2>&1 | tee /dev/tty | (! grep -q "NOT OPTIMIZED")

bench: compile
    ERL_LIBS=_build/default/lib/:_build/default/plugins test/bench/default-config.escript

test *ARGS:
    rebar3 ct --verbose --readable=false {{ ARGS }}

format:
    rebar3 fmt
