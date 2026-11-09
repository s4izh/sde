#!/usr/bin/env bash
# Run: bash ~/.local/scripts/lib/sde_test.sh
# shellcheck disable=SC2016  # snippets are expanded by the inner bash
set -uo pipefail

lib="$(dirname "$(readlink -f "$0")")/sde.sh"
failures=0

check() {
    local name=$1 expected_status=$2 expected_out=$3
    shift 3
    local out status
    out=$(bash -c "source '$lib'; $*" 2>&1)
    status=$?
    if [[ $status -ne $expected_status || "$out" != *"$expected_out"* ]]; then
        printf 'FAIL %s: status=%s out=%q\n' "$name" "$status" "$out"
        failures=$((failures + 1))
    else
        printf 'ok   %s\n' "$name"
    fi
}

check "info prints script name and level" 0 "bash: info: conectando" 'info conectando'
check "info keeps stdout clean" 0 "stdout=[]" 'out=$(info conectando 2>/dev/null); echo "stdout=[$out]"'
check "warn prints script name and level" 0 "bash: warning: hola" 'warn hola'
check "die exits 1 with fatal message" 1 "bash: fatal: boom" 'die boom; echo unreachable'
check "die_usage exits 2 without level" 2 "usage: bash [a|b]" 'die_usage "[a|b]"; echo unreachable'
check "need passes for existing commands" 0 "" 'need bash ls'
check "need fails naming the missing command" 1 "missing command: no-such-cmd" 'need bash no-such-cmd'
check "need_env passes when set" 0 "" 'FOO=1 need_env FOO'
check "need_env fails naming the unset var" 1 "BAR not set" 'unset BAR; need_env BAR'
check "need_env treats empty as unset" 1 "BAR not set" 'BAR= need_env BAR'
check "tmux_has fails for a missing session" 1 "" 'TMUX_TMPDIR=$(mktemp -d) tmux_has sde-test-none'
check "tmux_go switches client inside tmux" 0 "switch-client -t s" 'tmux() { echo "$*"; }; TMUX=x tmux_go s'
check "tmux_go attaches outside tmux" 0 "attach-session -t s" 'tmux() { echo "$*"; }; unset TMUX; tmux_go s'

if [[ $failures -ne 0 ]]; then
    echo "$failures failed"
    exit 1
fi
echo "all passed"
