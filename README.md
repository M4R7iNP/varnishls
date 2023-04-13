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
```

#### Neovim lsp setup:

```lua
local lspconfig = require('lspconfig')
local lsp_configs = require('lspconfig.configs')

lsp_configs.varnishls = {
  default_config = {
    -- Change the path to varnishls (add --debug for debug log)
    cmd = { "/home/martin/varnishls/target/debug/varnishls", "lsp", "--stdio" },
    filetypes = { "vcl" },
    root_dir = function(fname)
      return lspconfig.util.root_pattern(".varnishls.toml")(fname) or lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
    end,
    settings = {},
  }
}
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

vim.filetype.add({ extension = { vcl = 'vcl', vtc = 'vtc' } })
```

Run `:TSInstallFromGrammar vcl` after adding the nvim-treesitter config.

### TODO:

- Support for `.vcc` (Varnish VMOD definition files) and `.vtc` (Varnish test files) files
- - Use the `.vcc` files for autocomplete with docs.
- Build with Github Actions
- When writing a subroutine, detect which builtin subroutine it's called from.

### Inspiration:

- [tree-sitter-c](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js)
- [tower-lsp-boilerplate](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate)
- [prosemd-lsp](https://github.com/kitten/prosemd-lsp)
- [svls](https://github.com/dalance/svls)
