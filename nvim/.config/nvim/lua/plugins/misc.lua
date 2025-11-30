return {
  { 'github/copilot.vim' },
  { 'Raimondi/delimitMate' },
  {
    'terrortylor/nvim-comment',
    main = 'nvim_comment',
    opts = { comment_empty = false },
    config = true
  },
  {
    'lambdalisue/vim-suda',
    name = 'suda',
    keys = {
      { 'w!!', 'SudaWrite', mode = 'c' }
    }
  },
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      { 'hrsh7th/cmp-cmdline' },
      { 'hrsh7th/cmp-buffer' },
      { 'hrsh7th/cmp-path' }
    },
    config = function()
      local cmp = require 'cmp'
      cmp.setup {
        mapping = cmp.mapping.preset.insert {
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<Up>'] = cmp.mapping.select_prev_item { behavior = 'select' },
          ['<Down>'] = cmp.mapping.select_next_item { behavior = 'select' },
          ['<CR>'] = cmp.mapping.confirm { select = false }
        },
        sources = cmp.config.sources {{ name = 'buffer' }}
      }

      cmp.setup.cmdline({ '/', '?' }, {
          mapping = cmp.mapping.preset.cmdline(),
          sources = {{ name = 'buffer' }}
      })

      cmp.setup.cmdline(':', {
          mapping = cmp.mapping.preset.cmdline(),
          sources = cmp.config.sources(
            {{ name = 'path' }},
            {{ name = 'cmdline' }}),
          matching = { disallow_symbol_nonprefix_matching = false }
      })
    end
  }
}
