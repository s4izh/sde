# Architecture Decision Records

One file per decision, numbered in order. A record is never edited after it is
accepted: if a decision changes, write a new ADR and mark the old one as
`Superseded by NNNN`.

| ADR | Title | Status |
|-----|-------|--------|
| [0001](0001-dotfiles-as-plain-symlinks.md) | Dotfiles as plain symlinks, not home-manager | Accepted |
| [0002](0002-per-host-config-via-hostname-links.md) | Per-host config via hostname-selected links | Accepted |

## Template

Copy this into `NNNN-short-title.md`:

```markdown
# NNNN. Title

- Status: Proposed | Accepted | Superseded by NNNN
- Date: YYYY-MM-DD

## Context

What problem or situation forces a decision.

## Decision

What we do.

## Consequences

What gets easier, what gets harder, what to watch out for.
```
