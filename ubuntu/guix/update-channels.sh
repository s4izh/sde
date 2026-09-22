#!/usr/bin/env bash
# Update channels: pull the unlocked channel list, then re-lock it by
# capturing the resulting commits with `guix describe -f channels`.
#
# ~/.config/guix/channels.scm is expected to be a symlink to
# channels-locked.scm (see README.md), so a plain `guix pull` always
# rebuilds from the locked commits unless this script is run.
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

guix pull -C channels-non-locked.scm
guix describe -f channels > channels-locked.scm
guix pull -C channels-locked.scm

echo "Locked channels updated. Review and commit channels-locked.scm:"
git diff --stat -- channels-locked.scm
