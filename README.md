```
npm i
npx tree-sitter generate
cargo run --bin lsp
```

Vim lsp setup:

```lua
local lspconfig = require 'lspconfig'
local lsp_configs = require('lspconfig.configs')

lsp_configs.vcl = {
  default_config = {
    -- Update the path to vcl-lsp
    cmd = { "/home/martin/vcl-parser/target/debug/lsp", "--stdio" },
    filetypes = { "vcl" },
    root_dir = function(fname)
      return lspconfig.util.find_git_ancestor(fname) or vim.fn.getcwd()
    end,
    settings = {},
  }
}

```

Inspiration:

- [tree-sitter-c](https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js)
- [tower-lsp-boilerplate](https://github.com/IWANABETHATGUY/tower-lsp-boilerplate)
- [prosemd-lsp](https://github.com/kitten/prosemd-lsp)
