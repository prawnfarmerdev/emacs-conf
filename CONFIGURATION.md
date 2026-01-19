# Modular Emacs Configuration

## Structure

```
~/.emacs.d/
├── init.el                    # Main loader (minimal)
├── CONFIGURATION.md          # This file
└── config/                    # Modular configuration files
    ├── core.el               # Package management, UI, editor behavior, fonts
    ├── my-evil.el            # Evil mode & evil-collection
    ├── completion.el         # Vertico, orderless, consult, embark, marginalia
    ├── helpers.el            # Helper functions (directory, search, terminal)
    ├── git.el                # Git configuration (Magit)
    ├── my-eshell.el          # Enhanced eshell configuration
    ├── my-perspective.el     # Workspace management with custom switcher
    ├── my-tab-line.el        # Tab-bar integration with perspective
    ├── my-fzf.el             # FZF fuzzy finder with directory functions
    ├── keybindings.el        # Tmux-style keybindings (depends on helpers)
    ├── theme.el              # Solarized theme & face customizations
    ├── languages.el          # Programming language modes
    └── windows.el            # Windows-specific overrides (conditional)
```

## Load Order & Dependencies

1. **core.el**: Platform detection, package management, UI, editor behavior, fonts
2. **my-evil.el**: Evil mode (Vim keybindings)
3. **completion.el**: Completion framework (vertico, orderless, consult, embark)
4. **helpers.el**: Helper functions (my/current-dir, my/open-eshell-here, etc.)
5. **git.el**: Git configuration (Magit) with lazygit-like keybindings
6. **my-eshell.el**: Enhanced eshell configuration
7. **my-perspective.el**: Perspective workspace management
8. **my-tab-line.el**: Tab-line with perspective integration
9. **my-fzf.el**: FZF fuzzy finder (depends on perspective and helpers)
10. **keybindings.el**: Tmux-style keybindings (depends on all above)
11. **theme.el**: Solarized theme & face customizations
12. **languages.el**: Programming language modes
13. **windows.el**: Windows-specific overrides (loaded conditionally)

## Platform Support

### Linux/macOS
- Font: "CaskaydiaCove Nerd Font"
- Directory candidates: ~/files, ~/projects, ~/.config, ~/.local, /home
- External tools: fzf, ripgrep (rg), grep

### Windows
- Font: "Cascadia Code"
- Directory candidates: ~/Documents, ~/Projects, ~/Desktop, C:/Users, C:/Projects
- External tools: fzf.exe, rg.exe, grep.exe
- Path separator adjustments

Windows configuration is loaded conditionally when `my/is-windows` is true.

## Key Features Preserved

- **Perspective workspaces**: `C-SPC s` full-screen switcher (RET switch, x delete, g refresh, q quit)
- **Tmux-style keybindings**: `C-SPC` leader with window splits (`"`, `%`, `q`, `z`)
- **Search**: `C-SPC /` regexp search, `C-SPC R` toggle regexp/fixed
- **Enhanced eshell**: `C-SPC t e` opens enhanced eshell with git prompt, fish completion, syntax highlighting, aliases, and persistent history
- **Eat terminal**: `C-SPC t t` opens eat terminal with excellent Windows SSH support and pseudo-terminal emulation
- **Tabs**: Numbered tab-line showing perspective-filtered file buffers (`C-SPC 1-9`)
- **Fuzzy finder**: `C-f` directory selection with perspective creation
- **File operations**: `C-SPC f` prefix for file/directory operations
- **Git integration**: `C-SPC g` prefix for Magit operations (status, dispatch, blame, log, commit, push, pull, stash, branch, merge, rebase, fetch, remote)

## Performance

- **Startup timing**: Configuration load time is measured and displayed on startup (approx 1 second).
- **Lazy loading**: Heavy packages like Magit and which-key are deferred until first use.

## Known Issues

1. **Perspective helper functions**: Fixed by adding `:demand t` to the perspective `use-package` declaration. Functions now load correctly in batch mode.

2. **Windows testing**: Windows-specific configuration has not been tested on Windows.

## Migration Notes

- Original monolithic `init.el` backed up as `init.el.backup`
- All functionality from original configuration preserved
- Cross-platform compatibility maintained with eshell terminal
- Modular structure enables easier maintenance and platform-specific customization

## Testing

To test the configuration:

```bash
# Basic load test
emacs --batch --eval "(load-file \"~/.emacs.d/init.el\")"

# Function availability test
emacs --batch --eval "(load-file \"~/.emacs.d/init.el\")" --eval "(if (fboundp 'my/emacs-sessionizer) (message \"OK\") (message \"FAIL\"))"
```

## Adding New Modules

1. Create new `.el` file in `config/` directory
2. Add `(provide 'module-name)` at end
3. Update load order in `init.el` as needed
4. Ensure dependencies are satisfied (load after required modules)