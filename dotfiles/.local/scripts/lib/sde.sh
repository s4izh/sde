# shellcheck shell=bash
# source "$HOME/.local/scripts/lib/sde.sh"

# Messages go to stderr as "script: level: message" (git style), so stdout
# stays clean for pipes and levels can be grepped. info shadows texinfo's
# info(1) in scripts that source this file.
_sde_msg() {
    local level=$1
    shift
    printf '%s: %s: %s\n' "${0##*/}" "$level" "$*" >&2
}
info() { _sde_msg info "$@"; }
warn() { _sde_msg warning "$@"; }
die() { _sde_msg fatal "$@"; exit 1; }
die_usage() { printf 'usage: %s %s\n' "${0##*/}" "$*" >&2; exit 2; }

need() {
    local cmd
    for cmd; do
        command -v "$cmd" >/dev/null || die "missing command: $cmd"
    done
}

need_env() {
    local var
    for var; do
        [[ -n "${!var:-}" ]] || die "$var not set"
    done
}

tmux_has() { tmux has-session -t "=$1" 2>/dev/null; }

tmux_go() {
    if [[ -n "${TMUX:-}" ]]; then
        tmux switch-client -t "$1"
    else
        tmux attach-session -t "$1"
    fi
}
