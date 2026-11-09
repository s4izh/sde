# Multi-agent workflow: Multica + Hermes + Claude Code on NixOS

Status: applied. `home/sergio/agents.nix` (option `sde.agents.enable`, on
for rx), `hermes-agent` flake input, `users.users.sergio.linger` on rx,
wrappers in `dotfiles/.local/scripts`. The DirectSDK plugin is installed
declaratively (`services.hermes-agent.extraPlugins`, pinned to the catalog
SHA), so `hermes plugins install` is not needed. Imperative steps left:
`multica` install and logins. The model is set in Nix (`settings.model`), because `hermes model` cannot save under HM.
Verified against: Multica `v0.8.0` source, Hermes Agent `main` (its own
`flake.nix`), `@agentclientprotocol/claude-agent-acp`, Claude Code docs
(Nov 2026). Re-check the "Facts that move" section after upgrades.

---

## 0. Corrections to the original brief

The brief assumed one ACP bus between every component. The code says otherwise:

| Assumption | Reality (source) | Consequence |
|---|---|---|
| Multica talks ACP to Claude via `claude-agent-acp` | Multica spawns `claude -p --input-format stream-json --output-format stream-json --permission-mode bypassPermissions` directly (`server/pkg/agent/claude.go`, `buildClaudeArgs`) | `claude-agent-acp` is **not** used by Multica. Keep it for ACP *editors* (Zed, Emacs agent-shell, VS Code ACP). |
| You register arbitrary ACP agents in Multica settings | Runtimes are **built-in backends**, auto-detected on `PATH`; only the binary is overridable (`MULTICA_HERMES_PATH`, `MULTICA_CLAUDE_PATH`, …) plus per-agent `custom_args` | "Registration" = put the binary on the daemon's `PATH` (or set the env var) and restart the daemon. |
| Multica is the per-tool-call approval center | Daemon is headless. Hermes is launched with `HERMES_YOLO_MODE=1` and the daemon **auto-answers** `session/request_permission` (`hermes.go`). Claude runs in `bypassPermissions` with `AskUserQuestion` disabled. | Human-in-the-loop happens at the **issue** level (assign, comment/steer, review the diff/PR), not per shell command. Sandbox accordingly (§6). |
| Hermes uses the Claude Pro/Max subscription as its backend | **Possible**, via Nous' official plugin *Claude Subscription DirectSDK (Experimental)* (`hermes plugins install claude-subscription-directsdk`, catalog v0.3.4). Hermes keeps its agent loop/tools/memory; each model call spawns the **unmodified** `claude` CLI (`-p`, stream-json) logged in with your account. The plugin never reads your credential files. | Use this plugin. Avoid the two other routes: Hermes' built-in `anthropic` OAuth provider (Max + bought extra credits only; bills only overage) and the third-party `hermes-claude-auth` patch (defeats Anthropic's OAuth validation). |

The architecture below uses only supported paths. One caveat on the
subscription route: Anthropic allows "an end user signing in to the
unmodified Claude Code binary with their own Claude subscription" and says
Agent SDK / `claude -p` / third-party-app usage "draw from your subscription
limits" (support article, Oct 2026). It does not name this plugin, the plugin
calls itself experimental, and Anthropic "reserves the right to take measures
to enforce" its restrictions. Personal use only, never a shared or team setup.

---

## 1. Architecture and data flow

```
            ┌──────────────────────────── Multica server ─────────────────────────────┐
            │  (Multica Cloud, or self-hosted: Go API + Postgres 17, Docker)           │
            │  issues · board · agents · runtimes · comments · run transcripts        │
            └───────────────▲─────────────────────────────────────────────────────────┘
                            │ WebSocket (task assignment, streamed events, steering)
┌───────────────────────────┴───────────────────────────────────────────────────────────┐
│ rx (NixOS)   systemd --user: multica-daemon.service  (`multica daemon start --fg`)   │
│                                                                                       │
│   per task: git worktree under ~/multica_workspaces/<ws>/…                            │
│                                                                                       │
│   ┌──────────── Runtime "hermes" ───────────┐   ┌──────── Runtime "claude" ─────────┐  │
│   │ spawn: hermes [custom_args] acp         │   │ spawn: claude -p                  │  │
│   │ env:   HERMES_YOLO_MODE=1               │   │   --input/--output-format         │  │
│   │ proto: ACP, JSON-RPC 2.0 over stdio     │   │     stream-json                   │  │
│   │   daemon → initialize, session/new,     │   │   --permission-mode               │  │
│   │            session/prompt, cancel       │   │     bypassPermissions             │  │
│   │   agent  → session/update (chunks,      │   │ proto: NDJSON over stdio          │  │
│   │            tool_call, plan),            │   │ auth:  your Claude subscription   │  │
│   │            request_permission ──auto-OK │   │        (CLAUDE_CONFIG_DIR=        │  │
│   │ state: ~/.hermes/state.db, memory,      │   │         ~/.claude-personal)       │  │
│   │        skills                           │   │ state: ~/.claude-personal/        │  │
│   │ model: DirectSDK plugin; per call it    │   │                                   │  │
│   │   spawns `claude -p` (stream-json,      │   │                                   │  │
│   │   CC tools off, tmp cwd,                │   │                                   │  │
│   │   CLAUDE_CONFIG_DIR=~/.claude-personal) │   │                                   │  │
│   └──────────────┬──────────────────────────┘   └──────────────┬────────────────────┘  │
└──────────────────┼─────────────────────────────────────────────┼───────────────────────┘
                   │ HTTPS, your Claude login (plan limits)       │ HTTPS, same login
                   ▼                                              ▼
                        api.anthropic.com  (Claude models)

Hermes owns tools, memory and approvals; `claude` is only its model client
there. Both runtimes draw from the same plan limits.

Separately, interactive use from an editor (you are the approver here):
   Emacs/Zed (ACP client) ──stdio ACP──▶ hermes acp            (approval prompts shown)
                          ──stdio ACP──▶ claude-agent-acp ──▶ claude (your npm install)
```

### Message passing

* **Multica ↔ daemon**: WebSocket. The server pushes "run task T for agent A on
  runtime R"; the daemon streams back assistant text, tool calls, usage, final
  status. Comments posted on a running issue are forwarded as *steering*
  (Claude Code supports mid-run steering; Hermes gets them on the next turn).
* **Daemon ↔ Hermes (ACP)**: JSON-RPC 2.0, newline-delimited, over the child's
  stdin/stdout. Hermes logs to **stderr only**; stdout is reserved for protocol
  frames — never wrap `hermes` in a script that prints to stdout.
  Sequence: `initialize` → `session/new {cwd, mcpServers}` (Multica forwards its
  MCP servers here) → `session/prompt` → stream of `session/update`
  notifications → `stopReason`. Hermes binds file/terminal tools to the `cwd`
  from `session/new`, i.e. the task's worktree.
* **Daemon ↔ Claude Code**: Claude Code's own stream-json protocol, not ACP.

### Tool approvals

| Path | Who approves |
|---|---|
| Multica → Hermes | Daemon auto-selects a grant option (`allow_once`/`allow_session`) for every request, incl. Hermes' file-edit guard. Effectively unattended. |
| Multica → Claude | `bypassPermissions`: no prompts at all. |
| Editor → Hermes (`hermes acp`) | You: `allow_once` / `allow_session` / `allow_always` (persisted) / `deny`. Timeout `approvals.timeout` (300 s) = deny. |
| Editor → `claude-agent-acp` | You, per Claude Code permission mode and your `settings.json` rules. |

So the review gate for delegated work is the **diff**, not the tool call.

### State persistence

| Component | Where | Notes |
|---|---|---|
| Multica | server DB (issues, runs, transcripts); `~/.multica/` (daemon config, pid, logs, profiles) | `multica config show` prints the config path |
| Workspaces | `~/multica_workspaces` (`MULTICA_WORKSPACES_ROOT`) | per-task checkouts; prune occasionally |
| Hermes | `~/.hermes/{config.yaml,.env,auth.json,state.db,skills/,memory}` | sessions survive restarts; long-term memory is shared between CLI, ACP and Multica runs |
| Claude Code | `~/.claude-personal/` (credentials, settings, sessions, memory) | Multica resumes sessions with `--resume <id>` |

Nothing mutable lives in `/nix/store`; Nix provides binaries + the declarative
parts of `config.yaml`, everything else is runtime state under `$HOME`.

---

## 2. NixOS / Home Manager setup

### What comes from where

| Tool | Source | Why |
|---|---|---|
| `claude` (Claude Code) | npm, `~/.npm` (existing) | as today; self-updating or pinned by you |
| `claude-agent-acp` | npm, `~/.npm` | same toolchain as `claude`; only needed for editors |
| `hermes`, `hermes-acp` | Hermes' own flake (`NousResearch/hermes-agent`), HM module | uv2nix build, read-only store safe; not in nixpkgs |
| `multica` | upstream release binary (Go) or `buildGoModule` | not in nixpkgs |
| `nodejs`, `git`, `gh`, build tools | nixpkgs | `nodejs` already in `modules/desktop.nix` |

### 2.1 npm side: global installs, updated by hand

Agents run the binaries in `~/.npm/bin` directly. No `npx` in the wrappers:
an unversioned `npx` reuses its cache forever (it was stuck on claude-code
2.1.202 / claude-agent-acp 0.63.0), and `@latest` checks the registry on
every spawn, which costs about 3 s and fails offline. Hermes' plugin spawns
`claude` on every model call, so that delay would hit every turn.

Updates are explicit: `make npm-update` (in `nixos/`) runs
`npm install -g <pkg>@latest` for every entry in `NPM_GLOBALS` (`config.mk`).
Claude Code's own auto-updater may also update `~/.npm` when you use it
interactively. The DirectSDK plugin disables it for the copies it spawns.

```bash
# dotfiles/.local/scripts/claude
exec env CLAUDE_CONFIG_DIR="$HOME/.claude-personal" "$HOME/.npm/bin/claude" "$@"
```

```bash
# dotfiles/.local/scripts/claude-agent-acp
export CLAUDE_CONFIG_DIR="$HOME/.claude-personal"
export CLAUDE_CODE_EXECUTABLE="$HOME/.npm/bin/claude"   # my CLI, not the bundled one
exec "$HOME/.npm/bin/claude-agent-acp" "$@"
```

The old wrappers also lacked `exec` and quoting (`$@`), and the ACP one did
not set `CLAUDE_CONFIG_DIR`, so it used the `~/.claude` login.

### 2.2 Flake input for Hermes

```nix
# flake.nix — inputs
hermes-agent = {
  url = "github:NousResearch/hermes-agent";
  inputs.nixpkgs.follows = "nixpkgs";
  inputs.home-manager.follows = "home-manager";
};
```

`inputs` already reaches Home Manager through `extraSpecialArgs`, so no
plumbing change is needed.

### 2.3 Home Manager module (e.g. `home/sergio/agents.nix`)

```nix
{ config, lib, pkgs, inputs, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;
  # The default Hermes build ships without the ACP extra, and lazy installs
  # can't write to /nix/store — so add the group at build time.
  hermes = inputs.hermes-agent.packages.${system}.default.override {
    extraDependencyGroups = [ "acp" ];
  };
  home = config.home.homeDirectory;
in
{
  imports = [ inputs.hermes-agent.homeManagerModules.default ];

  # `hermes` + `hermes-acp` on PATH, HERMES_HOME exported.
  programs.hermes-agent = {
    enable = true;
    package = hermes;
  };

  # Only for its activation script that writes config.yaml / .env.
  # gateway stays off: Multica and editors spawn `hermes acp` per session,
  # there is no long-running Hermes service in this design.
  services.hermes-agent = {
    enable = true;
    package = hermes;
    gateway.enable = false;
    # config.yaml is managed by HM, so `hermes model` can't save: set it here.
    settings = {
      model = {
        provider = "claude-subscription-directsdk-experimental";
        default = "claude-sonnet-5-5[1m]";   # `hermes model` lists the options
      };
      approvals.timeout = 300;           # editor sessions only
      terminal.backend = "local";
      plugins.entries.claude-subscription-directsdk-experimental.settings = {
        claude_config_dir = "${home}/.claude-personal";
      };
    };
    environment = {
      # Your npm claude; the plugin otherwise searches PATH + usual prefixes.
      CLAUDE_SUBSCRIPTION_DIRECTSDK_COMMAND = "${home}/.npm/bin/claude";
    };
    # No ANTHROPIC_API_KEY: the plugin refuses to start if one is inherited.
  };

  home.packages = with pkgs; [ git gh ripgrep ];

  # Variables for units started by systemd --user (the Multica daemon).
  systemd.user.sessionVariables = {
    DISABLE_AUTOUPDATER = "1";   # optional: pin claude to what you `npm i -g`'d
  };
}
```

Then `imports = [ ./agents.nix ];` in `home/sergio/home.nix`, and
`users.users.sergio.linger = true;` in `hosts/rx/default.nix` so user units
keep running after logout / start at boot.

Check after `make`/switch:

```sh
hermes acp --check        # ACP extra importable, config readable
```

> If `extraDependencyGroups` is rejected (Hermes reshuffles these), check
> `nix/packages.nix` in the Hermes repo; the fallback is
> `services.hermes-agent.extraPythonPackages` with `agent-client-protocol`.

### 2.4 Environment flags that matter

| Variable | Set where | Purpose |
|---|---|---|
| `CLAUDE_CONFIG_DIR=~/.claude-personal` | wrappers | which Claude login/settings every path uses |
| `CLAUDE_CODE_EXECUTABLE=~/.npm/bin/claude` | `claude-agent-acp` wrapper | adapter drives your CLI, not its bundled one |
| `DISABLE_AUTOUPDATER=1` | optional | with npm-global the prefix is writable, so auto-update *works*; set this only if you want versions to change only when you run `npm i -g`. The DirectSDK plugin always sets it for the `claude` it spawns. |
| `CLAUDE_SUBSCRIPTION_DIRECTSDK_COMMAND` | HM `services.hermes-agent.environment` | which `claude` Hermes' plugin spawns |
| `ANTHROPIC_API_KEY` | **must be unset** for Hermes | the plugin rejects inherited API-key / custom-endpoint overrides |
| `HERMES_HOME` | HM module | exported automatically |
| `MULTICA_CLAUDE_PATH`, `MULTICA_HERMES_PATH` | `multica config set` / unit env | which binaries the daemon spawns |
| `MULTICA_WORKSPACES_ROOT` | same | where task checkouts go |

### 2.5 Authentication (non-declarative, by design)

Tokens are runtime state; they must never be Nix expressions (the store is
world-readable).

```sh
# Claude Code — subscription OAuth, via Anthropic's own flow.
# Used by BOTH runtimes (Hermes' plugin reuses this login, never copies it).
claude auth login     # wrapper sets CLAUDE_CONFIG_DIR=~/.claude-personal
                      # (or: claude, then /login)

# Hermes — no secret of its own on this route

# Multica
multica login   # or: multica setup  (cloud) / multica setup self-host
```

If you later add an API-key provider to Hermes (e.g. a fallback), put the
key in a 0600 file and list it in `services.hermes-agent.environmentFiles`
(or a sops-nix/agenix path), never in `environment`.

---

## 3. Hermes configuration

1. **Provider: Claude subscription through the DirectSDK plugin.** The plugin
   goes in `~/.hermes/plugins/` (state, not the Nix store), pinned to a
   reviewed commit by the Hermes catalog. It needs Hermes ≥ 0.21.4.
   ```sh
   claude --version                         # opus alias needs ≥ 2.1.280, haiku ≥ 2.1.293
   # (already installed by Nix via extraPlugins; otherwise:
   #  hermes plugins install claude-subscription-directsdk)
   hermes model                             # only to LIST models; it can't save under HM,
                                            # set model.{provider,default} in agents.nix
   hermes chat -q "say ok" -Q               # one round trip through `claude -p`
   ```
   Behaviour worth knowing:
   * one fresh `claude` process per model call, in a private temp dir.
     Claude Code's own tools, skills, CLAUDE.md and auto-memory are **off**.
     Hermes owns tools, approvals, memory and compaction.
   * usage counts against your plan limits; `/usage` in Hermes asks Claude
     Code. Turn off *extra usage* on the account if you don't want overage.
   * the context limit is 200K on this route (the plugin README reports a run
     that died at 212K). Hermes compaction must kick in before that.
   * it fails closed: no silent fallback to an API key. A missing `claude`
     returns 503 and a missing login 401.
   * optional: `hermes config set plugins.entries.claude-subscription-directsdk-experimental.settings.claude_code_telemetry false`
   * **if the plugin breaks** (experimental): the fallback is
     `ANTHROPIC_API_KEY` via `environmentFiles` + `hermes model` → Anthropic,
     billed per token.
2. **Toolset for ACP** (optional, in `settings`):
   ```nix
   platform_toolsets.acp = [ "file" "terminal" "web" "skills" "memory" "todo" ];
   agent.disabled_toolsets = [ "code_execution" ];
   ```
   Default `hermes-acp` toolset: file ops, terminal/process, web/browser,
   memory, todo, session search, skills, `execute_code`, `delegate_task`,
   vision. The browser tool needs `hermes acp --setup-browser` (~400 MB
   Chromium download into `~/.hermes`, not the store).
3. **Run it in ACP mode by hand** to see the protocol:
   ```sh
   hermes acp 2>/tmp/hermes.log
   # paste:
   {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":1,"clientCapabilities":{}}}
   ```
   You should get an `initialize` result on stdout and logs in the file.
4. **Editor use** (Emacs agent-shell / Zed): command `hermes`, args `["acp"]`.

---

## 4. Multica configuration

### 4.1 Install the CLI/daemon

Packaged in `pkgs/multica.nix` (upstream release tarball, a statically linked
Go binary, so no patchelf is needed; `nix build .#multica`). `agents.nix` adds
it to `home.packages` and the daemon unit runs it from the store. To upgrade,
bump `version` and the two hashes (`nix hash file --sri <tarball>`);
`multica update` can't write to the store. The license is Apache-2.0 plus
extra conditions, so it is marked unfree. HM uses the system pkgs
(`home-manager.useGlobalPkgs` in `base.nix`), which has `allowUnfree`. Don't use the upstream `install.sh`: it targets
`/usr/local/bin`.

Server side: use **Multica Cloud** to start. Self-hosting needs Docker +
Postgres 17 (`multica setup self-host`); on NixOS enable
`virtualisation.docker` (see `modules/virtualisation.nix`), or translate the
compose file to `virtualisation.oci-containers` + `services.postgresql`.

### 4.2 "Register" the runtimes

There is no "add custom ACP agent" form. The daemon detects known CLIs on
its `PATH` and offers each as a runtime. Point it at the wrappers explicitly
so a systemd-launched daemon behaves like your shell:

```sh
multica config set MULTICA_CLAUDE_PATH ~/.local/scripts/claude   # → ~/.claude-personal login
multica config set MULTICA_HERMES_PATH ~/.nix-profile/bin/hermes
multica daemon status    # lists detected agents + versions (claude ≥ 2.0.0 required)
```

(If `config set` doesn't accept these keys in your version, set them as
`Environment=` in the unit below instead.)

Then in the web UI: **Agents → New agent**, pick runtime `hermes` or
`claude`, model, and optional `custom_args` (Hermes: e.g. `-p research` for a
Hermes profile; Claude: e.g. `--add-dir`). The daemon strips args that would
break the protocol (`acp`, `--output-format`, `--permission-mode`, …).

### 4.3 systemd user service, declaratively

`multica daemon autostart enable` writes `~/.config/systemd/user/multica-daemon.service`
imperatively and captures your *current* `PATH`. Do the same thing in Home
Manager so it's reproducible:

```nix
systemd.user.services.multica-daemon = {
  Unit = {
    Description = "Multica agent runtime daemon";
    After = [ "network-online.target" ];
    Wants = [ "network-online.target" ];
    StartLimitIntervalSec = 120;
    StartLimitBurst = 5;
  };
  Service = {
    Type = "simple";
    ExecStart = "${lib.getExe multica} daemon start --foreground"; # multica = pkgs/multica.nix
    Environment = [
      "PATH=${lib.makeBinPath [ pkgs.git pkgs.gh pkgs.nodejs pkgs.coreutils pkgs.bash ]}:${config.home.homeDirectory}/.local/scripts:${config.home.homeDirectory}/.npm/bin:${config.home.profileDirectory}/bin:/run/current-system/sw/bin"
      "MULTICA_CLAUDE_PATH=${config.home.homeDirectory}/.local/scripts/claude"
      "MULTICA_HERMES_PATH=${config.home.profileDirectory}/bin/hermes"
    ];
    Restart = "on-failure";
    RestartSec = 10;
  };
  Install.WantedBy = [ "default.target" ];
};
```

Don't also run `multica daemon autostart enable`, or you'll get two units.
There is no separate "background ACP agent" service: ACP agents are
**per-session children** of the daemon (or of your editor) and exit when the
session ends. Keeping them resident would only hold memory and stale state.

```sh
systemctl --user status multica-daemon
journalctl --user -u multica-daemon -f      # or: multica daemon logs -f
```

---

## 5. Worked example: issue → Hermes → patch

Goal: "Add a `docs/agents.md` link to `README.md`" in this repo.

1. **Workspace + repo.** In Multica, workspace "sde", connect the `s4izh/sde`
   repo. `multica daemon status` on rx shows `hermes` and `claude` runtimes
   as online.
2. **Agent.** Create agent *hermes-dev*: runtime `hermes`, model per
   `config.yaml`. Create *claude-dev*: runtime `claude`, model
   `claude-opus-5-5` — the subscription-backed one.
3. **Issue.** "README: link docs/agents.md under Documentation", acceptance:
   "one-line change, no other edits". Assign to *hermes-dev*.
4. **Dispatch.** The server pushes the task; the daemon creates a checkout
   under `~/multica_workspaces/sde/…`, spawns
   `hermes acp` with `HERMES_YOLO_MODE=1`, sends `initialize`,
   `session/new {cwd: <checkout>, mcpServers: [multica…]}`, then
   `session/prompt` containing the issue.
5. **Observe.** The issue's run view streams `session/update` events:
   `tool_call search_files "Documentation"` → `read_file README.md` →
   `patch README.md`. Hermes' edit guard sends `request_permission`; the
   daemon auto-picks `allow_once` and it's logged. On the host:
   `journalctl --user -u multica-daemon -f` shows the same.
6. **Steer.** Comment "use a relative link". The daemon forwards it; Hermes
   amends the patch on its next turn.
7. **Review — this is your approval gate.** Hermes commits on a branch and
   opens a PR (needs `gh` auth in the daemon's env) or leaves the diff in
   the worktree. Review it like a human's PR. Reassign the issue to
   *claude-dev* for a second-opinion review if wanted.
8. **Close.** Merge; mark done. Hermes keeps what it learned about the repo
   in `~/.hermes` memory for the next task.

---

## 6. Risks and guardrails

* **Unattended execution.** Both runtimes run with approvals off, as your
  user, on rx. Mitigations, cheapest first: give the daemon its own Unix user
  (separate HM profile, separate `~/.hermes`, no access to `~/personal`
  secrets, own `gh` token scoped to the repos); add
  `ProtectHome=tmpfs`/`BindPaths=` hardening to the unit; or run tasks on the
  `vm` host instead of your workstation.
* **Shared plan limits.** Hermes (through the plugin) and the Claude runtime
  draw from the same subscription. Advertised limits assume ordinary
  individual use, and Hermes replays the full history on each call, so
  heavy fan-out hits rate limits (429) quickly. Run one or two agents at a
  time.
* **Policy risk.** See §0. Keep it personal; re-read Anthropic's
  legal/compliance page and the support article on Agent SDK + plans when
  either one changes.
* **Version drift.** Multica rejects a `claude` older than its `MinVersions`.
  Hermes ACP pins `agent-client-protocol==0.9.0`; upgrade Hermes and Multica
  together.

## Known issue: Multica overlay vs Nix-managed Hermes

Multica runs every Hermes task with `HERMES_HOME` set to an overlay under
`~/.multica/hermes-sessions/…`, which symlinks most of `~/.hermes`,
**including HM's `.managed` marker**. A managed Hermes does not create its
home skeleton, so the task dies in `initialize` with
`HomeInitializationError: Required directory does not exist: …/cron`
(Multica shows "hermes initialize failed: hermes process exited").

Fix in `agents.nix`: `MULTICA_HERMES_PATH` points at a small wrapper
(`hermesForMultica`) that creates the missing subdirectories in
`$HERMES_HOME` and then execs the real `hermes`. The list mirrors
`_HERMES_HOME_SUBDIRS` in `hermes_cli/config.py`; if a Hermes upgrade adds
one, the same error comes back naming the new directory. Worth reporting
upstream to Multica (overlay should not mirror `.managed`, or should seed the
skeleton).

Daemon logs live in `~/.multica/daemon.log`, not in the journal; Hermes'
stderr appears there as `[hermes:stderr]`.

## Facts that move (re-verify on upgrade)

* Multica's Claude invocation flags — `server/pkg/agent/claude.go`
* Multica's Hermes auto-approve behaviour — `server/pkg/agent/hermes.go`
* Hermes package groups / HM options — `nix/packages.nix`, `nix/homeManagerModules.nix`
* Anthropic subscription/third-party policy — code.claude.com/docs/en/legal-and-compliance,
  support.claude.com/en/articles/15036540
* DirectSDK plugin — NousResearch/hermes-official-plugins `claude-subscription-directsdk/README.md`
  (min Claude Code versions, env vars, settings keys)
