## varnish_lsp

This is a Varnish VCL language server. Provides some autocomplete, jump to definition across files and basic linting. Made for Varnish 6.0 (plus), but it should work with other versions - except Fastly.

[![asciicast](https://asciinema.org/a/575554.svg)](https://asciinema.org/a/575554)

## Setup
```
make tree-sitter-vcl
make build
```

#### Config

```toml
# .varnish_lsp.toml in your workspace dir
main_vcl = "vg/varnish.vcl" # path to the main vcl file varnish uses
vmod_paths = ["/usr/lib/varnish-plus/vmods/"] # paths to directories containing your vmods (.so binaries)
```

#### Neovim lsp setup:

```lua
local lspconfig = require('lspconfig')
local lsp_configs = require('lspconfig.configs')

lsp_configs.vcl = {
  default_config = {
    -- Change the path to varnish-lsp (add --debug for debug log)
    cmd = { "/home/martin/varnish-lsp/target/debug/varnish_lsp", "lsp", "--stdio" },
    filetypes = { "vcl" },
    root_dir = function(fname)
      return lspconfig.util.root_pattern(".varnish_lsp.toml")(fname) or lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
    end,
    settings = {},
  }
}
```

It is technically possible to use the tree-sitter grammar for syntax highlighting, but this is even more WIP than the lsp.
```lua
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.vcl = {
  install_info = {
    url = "~/<path to this repo>/vendor/tree-sitter-vcl",
    files = {"src/parser.c"},
  }
}

vim.filetype.add({ extension = { vcl = 'vcl' } })
```

Run `:TSInstallFromGrammar vcl` after adding the nvim-treesitter config.

### Inspiration:

- [tree-sitter-c](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js)
- [tower-lsp-boilerplate](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate)
- [prosemd-lsp](https://github.com/kitten/prosemd-lsp)
- [svls](https://github.com/dalance/svls)
