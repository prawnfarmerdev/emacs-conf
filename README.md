# Emacs Configuration

Modular Emacs configuration with Tmux/Vim-style keybindings.

## Features

- **Tmux-style workspace management**: Split windows, perspectives, tabs
- **Vim keybindings**: Evil mode with custom enhancements
- **Cross-platform**: Linux and Windows support with platform-specific overrides
- **Terminal support**: eshell (cross-platform) and eat (excellent Windows SSH support)
- **Fuzzy search**: Consult-based navigation (replaces FZF), vertico completion
- **SSH sessionizer**: Connect to servers from SSH config with perspective management
- **GitHub integration**: Quick PR creation, project initialization with gh CLI
- **Solarized theme**: Dark theme with pure black background
- **Custom mode-line**: Enhanced visibility with Solarized theme

## Keybindings

### Global Bindings (no prefix)
- **C-f**: Directory sessionizer (switch/create perspective)
- **C-S-f** (or **C-c C-s**): SSH sessionizer (connect to server from SSH config)
- **C-S-p**: Open GitHub PR for current branch
- **C-n**: Create new project with git & GitHub setup

### Leader Key: `C-SPC` (Ctrl+Space)
- **C-SPC /**: Search text in current directory
- **C-SPC R**: Toggle regex/fixed string search
- **C-SPC t e**: Open eshell terminal
- **C-SPC t t**: Open eat-eshell terminal (eat terminal emulation within eshell, recommended for Windows SSH)
- **C-SPC s**: Switch perspective (full screen)
- **C-SPC b**: Buffer management
- **C-SPC w**: Window management (split, close, navigate)
- **C-SPC p**: Perspective management
- **C-SPC g**: Git operations (Magit)
- **C-SPC f**: File operations (find, search, directories)

## Structure

- `init.el` - Main entry point, loads modules in order
- `config/` - Modular configuration files
  - `core.el` - Package management, UI, editor behavior, fonts
  - `my-evil.el` - Evil mode & evil-collection configuration
  - `completion.el` - Vertico, orderless, consult, embark, marginalia
  - `helpers.el` - Helper functions (directory, search, terminal, project creation)
  - `git.el` - Magit configuration with GitHub PR integration
  - `my-eshell.el` - Enhanced eshell configuration
  - `my-perspective.el` - Workspace management
  - `my-tab-line.el` - Tab-bar integration
  - `my-navigation.el` - Consult-based navigation (directory/file search)
  - `my-ssh.el` - SSH sessionizer with server inventory
  - `keybindings.el` - Tmux-style keybindings
  - `theme.el` - Solarized theme & mode-line customization
  - `languages.el` - Programming language modes
  - `windows.el` - Windows-specific overrides (loaded conditionally)
- `data/` - Data files (not tracked in git)
  -   `servers.csv.example` - Example CSV file for SSH sessionizer (optional primary source)

## Installation

1. Clone this repository to `~/.emacs.d` or symlink:

   ```bash
   git clone <repo-url> ~/.emacs.d
   ```

2. Start Emacs - packages will be installed automatically.

3. For Windows users, install:
   - [ripgrep for Windows](https://github.com/BurntSushi/ripgrep/releases) (required for text search)
   - [Git for Windows](https://gitforwindows.org/) (includes grep)
   - Note: FZF is no longer required - navigation uses Emacs Consult package

## Additional Setup

### SSH Sessionizer
1. **Primary source**: CSV file `~/.emacs.d/data/servers.csv` (optional, easier management)
   - CSV format: `hostname,name,username` (name optional, can be empty)
   - Also supports 2-column format: `hostname,username`
   - If CSV file doesn't exist, falls back to SSH config parsing
2. **SSH config file**: `~/.ssh/config` (automatically created with template if missing)
   - Standard SSH config syntax with `Host`, `HostName`, `User` directives
   - SSH config entries are generated on-demand when selecting servers from CSV
3. Use `C-S-f` (or `C-c C-s`) to connect to servers
4. Creates perspectives named after servers for workspace isolation
5. **TRAMP integration** (default): Opens remote directory via TRAMP (`/ssh:user@hostname:`)
   - Configure: `(setq my/ssh-use-tramp t)` (default) for TRAMP, `nil` for eshell SSH
   - Modes: `my/ssh-tramp-mode`: `'dired` (file browser) or `'shell` (remote shell)
6. **MFA support**: Configure authentication methods in SSH config (e.g., `PreferredAuthentications publickey,keyboard-interactive`)
7. **SSH key management**: Automatically detects existing SSH keys; generates new key if none exists (configurable)

### GitHub CLI (gh)
1. Install GitHub CLI: https://cli.github.com/
2. Authenticate: `gh auth login`
3. Use `C-S-p` to open PR for current branch
4. Use `C-n` to create new projects with automatic GitHub repo creation

## Platform Support

- **Linux**: Full support with Nerd Fonts
- **Windows**: Optimized with PowerShell integration, eshell terminal, Windows-specific paths

## Notes

- Terminals: eshell (shell shortcuts) and eat-eshell (eat terminal emulation within eshell)
- Evil mode is disabled in eshell buffers for shell compatibility
- Pure black background for terminal buffers
- Startup optimized with garbage collection tuning