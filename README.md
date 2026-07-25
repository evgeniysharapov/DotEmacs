# Emacs Configuration Based on `use-package` #

Motivational points:
  * Everything, including customization modules, is loaded using `use-package`. Because `use-package` wasn't a part of Emacs until 24.5 this configuration /as is/ is not working with 24.4. However it could be easily modified by just installing `use-package` via ELPA at the beginning.
  * Having configuration in `Emacs-Lisp`. I used to have literate programming style configuration using `org-babel`, but seems like complications: tricky navigation, swithing to source editing and back, reloading issues, performance hit outweigh the benefit of having a good documentation and code visibility manager. 

Since configuration is changing all the time, there's no point to describe it at length. Just some of the highlights.

## Some Packages/Tools ##

  * `ag` - I use my own custom `ag.el` because I need it to work on Windows
  * `browse-kill-ring`
  * `undo-tree`
  * `ace-window`
  * `ace-jump`
  * `ido` including `ido-ubiquitous` and `flx-ido`
  * `hide-lines`
  * `projectile` and `find-file-in-project`
  * `company`
  * `flycheck`
  * `paredit`
  * Python programming: `python-mode`, `anaconda`
  * C/C++: `cc-mode`, `c-company-headers`, `c-eldoc`
  * Javascript via `js-2`, `tern`, `mocha`, `js-comint`
  * Docker: `dockerfile-mode`, `yaml` and `docker`

## Language Server Support (eglot) ##

Language server integration uses the built-in `eglot` (Emacs 29+), not `lsp-mode`. Supporting pieces:

  * `eglot` - LSP client
  * `eglot-booster` - wraps servers with the `emacs-lsp-booster` binary for faster JSON-RPC (requires the binary on `PATH`, see below)
  * `flycheck-eglot` - routes eglot diagnostics into `flycheck`
  * `dape` - Debug Adapter Protocol client (replaces `dap-mode`)
  * `treesit-auto` - installs tree-sitter grammars and remaps classic major modes to their `*-ts-mode` counterparts

`eglot` keybindings live under the `C-z .` prefix:

  * `C-z . r` `eglot-rename`
  * `C-z . a` `eglot-code-actions`
  * `C-z . f` `eglot-format`
  * `C-z . d` `dape` (start debugger)

### Languages and Major Modes ###

Files open in tree-sitter major modes (grammars auto-installed on first use).

| Language   | Major mode       | Language server              |
|------------|------------------|------------------------------|
| Python     | `python-ts-mode` | `pyright-langserver`         |
| Rust       | `rust-ts-mode`   | `rust-analyzer`              |
| Go         | `go-ts-mode`     | `gopls`                      |
| JavaScript | `js-ts-mode`     | `typescript-language-server` |
| Java       | `java-ts-mode`   | `jdtls` (via `eglot-java`)   |
| YAML       | `yaml-ts-mode`   | `yaml-language-server`       |
| Shell/ZSH  | `bash-ts-mode`   | `bash-language-server`       |
| Terraform  | `terraform-mode` | `terraform-ls`               |
| C#         | `csharp-ts-mode` | `omnisharp` / `csharp-ls`    |
| F#         | `fsharp-mode`    | `fsautocomplete`             |

### System Prerequisites ###

These must be installed on the system and available on `PATH`. They are *not* installed by Emacs.

The booster binary (build with Rust/cargo):

```shell
cargo install --git https://github.com/blahgeek/emacs-lsp-booster --root ~/.cargo
```

Language servers:

```shell
# npm-based servers
npm i -g pyright typescript typescript-language-server \
         yaml-language-server bash-language-server

# Go and Rust (usually already present via toolchains)
go install golang.org/x/tools/gopls@latest
rustup component add rust-analyzer

# Terraform LS - from HashiCorp releases / package manager
# F# (optional)
dotnet tool install -g fsautocomplete
```

`jdtls` (Java) is downloaded and managed automatically by `eglot-java` on first use. Tree-sitter grammars are installed by `treesit-auto` (prompts on first visit to a file).

## Some Shortcuts ##

### Help  ###

  * `C-h M-k` describe keymap
  * `C-h C-c` describe character at point
  * `C-h C-b` describes keys customizations

### Some Useful Keymaps ###

These could be explored further via `C-h M-k` for example

  * `C-x f` file opening map
  * `C-x t` toggling keymap
  * `C-x w` windows management keymap
  * `C-z` personal keymap

### Buffer Visibility / Navigation ###
  * `C-z /` changes visibility via `hide-lines`
  * `M-s o` shows occurences of a string in a buffer (`occur`)
  * `M-s O` multi buffer occur (moccur)
  * `M-g j` starts `avy-jump`

### Miscellaneous  ###
  * `C-x K` kill this buffer
  * `C-M-:` start/switch to IELM with current buffer
  * `M-z` is zapping up to a character, `M-Z` zaps to character. Zapping backward is via negative universal argument.

asdfadsf

## TODO ##

* Using outline
    * ~~Faces for outline headers~~
    * ~~use outshine speedy~~
    * configure better keys like M-left, M-right, etc.
    * Configure folds
    
* Add ripgrep via deadgrep to M-s search keymap

* ~~Move all the parts configuration ffe-* files to init.el once outline is used.~~ 

* Heavy configuration parts can go to *-plus packages 

  

