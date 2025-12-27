compile:
    rebar3 compile

check-binaries:
    rebar3 as bin_opt_info compile 2>&1 | tee /dev/tty | (! grep -q "NOT OPTIMIZED")

erlperf: compile
    ERL_LIBS=_build/default/lib/ erlperf 'erl_nanoid:naive().'

test *ARGS:
    rebar3 ct --verbose --readable=false {{ ARGS }}
