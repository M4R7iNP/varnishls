# varnishls

`varnishls` is a Varnish Language Server. Provides some autocomplete, jump to definition across files and basic linting. Made for Varnish 6.0 (Enterprise), but it should work with other versions — except Fastly.

[![asciicast](https://asciinema.org/a/575554.svg)](https://asciinema.org/a/575554)

## Setup

* Download a release for your platform and put it somewhere, preferably in $PATH
* Either install varnish locally or download `VCC-files.zip` [here](https://github.com/auduny/vscode-vcl/releases/download/v0.10.0/varnish-vcc-files.zip) and place them in `$HOME/.varnishls/vcc`


```shell
mkdir $HOME/.varnishls
cd $HOME/.varnishls
unzip varnish-vcc-files.zip
```

* Set `VARNISHLS_VCC_PATHS` in `.profile`, `.zshrc` or `.bashrc`

```shell
export VARNISHLS_VCC_PATHS="./lib:./vcc:/usr/share/varnish/vcc:/opt/homebrew/opt/varnish/share/varnish/vcc:$HOME/.varnishls/vcc/"
```

Now setup up your editor of choice
* [VSCode](#VSCode)
* [Neovim](#Neovim)
* [Vim](#Vim)
* [Emacs](#Emacs)

### VSCode

Currently separate repo. Bundles varnishls.

> [!NOTE]
> VSCode extension does not need configuration, but will use it if it exists.

* [GitHub Repo](https://github.com/auduny/vscode-vcl)
* [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=audun-ytterdal.varnish-configuration-language)

### Neovim

Install [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)

Create a file in `~/.config/nvim/lsp/varnishls.lua`:

```lua
return {
  -- Point to varnishls (add --debug for debug log)
  cmd = { "varnishls", "lsp", "--stdio" },
  filetypes = { "vcl", "vtc" },
  root_markers = { ".varnishls.toml", ".git" },
  settings = {},
}
```

And add this in your `init.lua`:

```lua
vim.filetype.add({ extension = { vcl = 'vcl', vtc = 'vtc' } })
vim.lsp.enable('varnishls')
```

It is technically possible to use the tree-sitter grammar for syntax highlighting, but this is even more WIP than the lsp.

```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
for _, lang in pairs({ "vcl", "vtc" }) do
  parser_config[lang] = {
    install_info = {
      url = "/<path to this repo>/vendor/tree-sitter-" .. lang,
      files = {"src/parser.c"},
    }
  }
end
```

Run `:TSInstallFromGrammar vcl` after adding the nvim-treesitter config.

### Vim

vim-lsp using `prabirshrestha/vim-lsp`

```vim
au User lsp_setup call lsp#register_server({
        \ 'name': 'varnishls',
        \ 'cmd': {server_info->['/path/to/varnishls',"lsp","--stdio"]},
        \ 'allowlist': ['vcl','vtc'],
        \ })
```

### Emacs

Install vcl-mode and add this to `.emacs`

```lisp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(vcl-mode . ("varnishls" "lsp" "--stdio"))))
```

## Config Override

You can add `.varnishls.toml` in the root of your workspace and add overrides and tunings for specific Varnish configuration there.

```toml
# .varnishls.toml in your workspace dir
main_vcl = "vg/varnish.vcl" # path to the main vcl file varnish uses
vmod_paths = ["/usr/lib/varnish-plus/vmods/"] # paths to directories containing your vmods (.so binaries)
vcc_paths = ["/usr/src/varnish-cache/lib/"] # paths to directories containing vcc files (vmod definition files)
vcl_paths = ["./", "/usr/share/varnish-plus/vcl/"] # paths to directories containing vcl (default ./)
[lint]
enabled = true
prefer_else_if = "hint"
prefer_lowercase_headers = "hint"
prefer_custom_headers_without_prefix = false
invalid_regex = "error"
slow_regex = "warning"
[formatter]
enabled = true
indent_size = 4
format_large_ifs_style = "loose" # loose or tight
autofix_else_ifs = true
downcase_headers = true
```

Allowed levels for linting rules are error, warning, info, and hint. Set to false to disable a rule.

NOTE: `vcc_paths` takes precedence over `vmod_paths` when searching for vmods, since vcc also has documentation.


## Dev Setup

```
make tree-sitter-vcl tree-sitter-vtc
make build
```

## Inspiration

- [tree-sitter-c](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js)
- [tower-lsp-boilerplate](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate)
- [prosemd-lsp](https://github.com/kitten/prosemd-lsp)
- [svls](https://github.com/dalance/svls)
- [deno lsp](https://github.com/denoland/deno/tree/main/cli/lsp)
