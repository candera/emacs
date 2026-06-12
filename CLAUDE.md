# Craig Andera's Emacs Configuration

Personal Emacs config. This repo lives at `~/.emacs.d/custom` (symlinked or cloned there directly).

## File Structure

- `init.el` — Main configuration file (~5000+ lines). Organized into sections with comment banners.
- `early-init.el` — Early initialization (runs before init.el).
- `candera/` — Custom Elisp files:
  - `journal.el` — Daily journaling system (auto file creation, langtool spell/grammar check)
  - `journal.el` — Daily journaling system (auto file creation, langtool spell/grammar check)
  - `web-lookup.el` — Dictionary/Wikipedia lookup at point
  - `typing-speed.el` — Live WPM display in mode line
  - `rpg.el` — Fate dice roller
  - `outline-presentation.el` — Outline-based slide navigation
  - `view-visited-file.el` — Display/copy current file path

## Package Management

Three package managers are in use — respect whichever is already being used for a given package:

- **straight.el** — Preferred for packages needing git control or pinning
- **use-package** — Primary declaration macro for all packages
- **el-get** — Used for some packages (especially forks); being phased out where possible
- **package.el** — Fallback; MELPA, MELPA Stable, and org repos configured

When adding new packages, prefer `use-package` with `:ensure t` (package.el) or `:straight t`.

## Key Packages in Use

- **Org-mode** — Heavily customized; pinned to GNU ELPA. Extensive custom sort/agenda functions.
- **org-gcal** — Google Calendar sync for SCHEDULED/DEADLINE headings
- **ivy + counsel + prescient** — Completion framework (not helm, not vertico)
- **company** — Autocompletion (not corfu)
- **eglot** — LSP client (not lsp-mode)
- **magit + forge** — Git and GitHub/GitLab integration
- **paredit** — Structural editing for all Lisp modes
- **clojure-mode + inf-clojure** — Clojure development
- **projectile** — Project navigation
- **vterm / mistty** — Terminal emulation
- **langtool** — Grammar checking (used heavily in journal.el)
- **elfeed** — RSS reader
- **flyspell + flyspell-popup** — Spell checking

## Elisp Conventions

- No `lexical-binding: t` at the top of init.el — `lexical-let` is used for closures where needed
- Custom macro `when-not` is defined and used (equivalent to `unless`)
- Custom macro `comment` is defined (like Clojure's `comment` — discards body, returns nil)
- `encrypted-file-contents` helper reads GPG-encrypted files (used for API keys/tokens)
- Section headers use `;;;;;;...` banners with two blank lines between sections
- Prefer `add-hook` with lambdas over named hook functions for short setups

## Common Patterns

**Adding a package:**
```elisp
(use-package some-package
  :ensure t
  :init
  (setq some-package-option t)
  :config
  (some-package-setup))
```

**Adding a mode hook:**
```elisp
(add-hook 'some-mode-hook
          (lambda ()
            (do-something)))
```

**Keybindings** use `define-key` or `:bind` in `use-package`. Global keys often use `C-c` prefix for custom bindings.

## What to Watch Out For

- `init.el` is very long — use section banners to orient yourself. Search for `;;;;;;` to jump between sections.
- Some sections are commented out entirely (slack, sdcv) — they're disabled, not deleted, intentionally.
- `org-gcal` config references encrypted files; errors about missing files are normal outside the author's machine.
- `el-get-bundle` calls for custom forks may fail if the GitHub repo is unavailable.
- `when (= emacs-major-version 28)` guards exist for version-specific workarounds — don't remove them.
