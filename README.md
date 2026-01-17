# Emacs Configuration

Modular Emacs configuration with Tmux/Vim-style keybindings.

## Features

- **Tmux-style workspace management**: Split windows, perspectives, tabs
- **Vim keybindings**: Evil mode with custom enhancements
- **Cross-platform**: Linux and Windows support with platform-specific overrides
- **Eshell terminal**: Cross-platform terminal with regular shell shortcuts
- **Fuzzy search**: FZF directory search, consult/vertico completion
- **Solarized theme**: Dark theme with pure black background
- **Doom modeline**: Enhanced mode line with evil state display

## Keybindings

- **Leader key**: `C-SPC` (Ctrl+Space)
- **C-f**: Fuzzy find directories (FZF)
- **C-SPC /**: Search text in current directory
- **C-SPC g**: Toggle regex/fixed string search
- **C-SPC t**: Open terminal (eshell)
- **C-SPC s**: List buffers
- **C-SPC w**: Window management (split, close, navigate)

## Structure

- `init.el` - Main entry point, loads modules in order
- `config/` - Modular configuration files
  - `core.el` - Package management, UI, editor behavior, fonts
  - `my-evil.el` - Evil mode & evil-collection configuration
  - `completion.el` - Vertico, orderless, consult, embark, marginalia
  - `helpers.el` - Helper functions (directory, search, terminal)
  - `git.el` - Magit configuration
  - `my-eshell.el` - Enhanced eshell configuration
  - `my-perspective.el` - Workspace management
  - `my-tab-line.el` - Tab-bar integration
  - `my-fzf.el` - FZF fuzzy finder
  - `keybindings.el` - Tmux-style keybindings
  - `theme.el` - Solarized theme & doom-modeline
  - `languages.el` - Programming language modes
  - `windows.el` - Windows-specific overrides (loaded conditionally)

## Installation

1. Clone this repository to `~/.emacs.d` or symlink:

   ```bash
   git clone <repo-url> ~/.emacs.d
   ```

2. Start Emacs - packages will be installed automatically.

3. For Windows users, install:
   - [fzf for Windows](https://github.com/junegunn/fzf/releases)
   - [ripgrep for Windows](https://github.com/BurntSushi/ripgrep/releases)
   - [Git for Windows](https://gitforwindows.org/) (includes grep)

## Platform Support

- **Linux**: Full support with Nerd Fonts
- **Windows**: Optimized with PowerShell integration, eshell terminal, Windows-specific paths

## Notes

- Terminal uses eshell with regular shell shortcuts (Ctrl+a, Ctrl+e, etc.)
- Evil mode is disabled in eshell buffers for shell compatibility
- Pure black background for terminal buffers
- Startup optimized with garbage collection tuning