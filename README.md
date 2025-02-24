## varnishls

`varnishls` is a Varnish Language Server. Provides some autocomplete, jump to definition across files and basic linting. Made for Varnish 6.0 (plus), but it should work with other versions - except Fastly.

[![asciicast](https://asciinema.org/a/575554.svg)](https://asciinema.org/a/575554)

## Setup

```
make tree-sitter-vcl tree-sitter-vtc
make build
```

#### Config

```toml
# .varnishls.toml in your workspace dir
main_vcl = "vg/varnish.vcl" # path to the main vcl file varnish uses
vmod_paths = ["/usr/lib/varnish-plus/vmods/"] # paths to directories containing your vmods (.so binaries)
vcc_paths = ["/usr/src/varnish-cache/lib/"] # paths to directories containing vcc files (vmod definition files)
vcl_paths = ["./", "/usr/share/varnish-plus/vcl/"] # paths to directories containing vcl (default ./)
[lint]
prefer_else_if = "hint"
prefer_lowercase_headers = "hint"
prefer_custom_headers_without_prefix = false
```

Allowed levels for linting rules are error, warning, info, and hint. Set to false to disable a rule.

NOTE: `vcc_paths` takes precedence over `vmod_paths` when searching for vmods, since vcc also has documentation.

#### Neovim lsp setup:

```lua
local lspconfig = require('lspconfig')
local lsp_configs = require('lspconfig.configs')

lsp_configs.varnishls = {
  default_config = {
    -- Change the path to varnishls (add --debug for debug log)
    cmd = { "/home/martin/varnishls/target/debug/varnishls", "lsp", "--stdio" },
    filetypes = { "vcl", "vtc" },
    root_dir = lspconfig.util.root_pattern(".varnishls.toml", ".git"),
    settings = {},
  }
}

vim.filetype.add({ extension = { vcl = 'vcl', vtc = 'vtc' } })
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

### Inspiration:

- [tree-sitter-c](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js)
- [tower-lsp-boilerplate](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate)
- [prosemd-lsp](https://github.com/kitten/prosemd-lsp)
- [svls](https://github.com/dalance/svls)
- [deno lsp](https://github.com/denoland/deno/tree/main/cli/lsp)
