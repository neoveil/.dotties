return {
  'neovim/nvim-lspconfig',
  dependencies = {
    {
      'VonHeikemen/lsp-zero.nvim',
      branch = 'v3.x',
      config = function()
        local z = require 'lsp-zero'
        z.extend_lspconfig()

        z.on_attach(function(_, bufnr)
          z.default_keymaps { buffer = bufnr }
        end)

        z.set_sign_icons {
          error = '✘',
          warn = '▲',
          hint = '⚑',
          info = '»'
        }
      end
    },
    {
      'nvimdev/lspsaga.nvim',
      dependencies = {
        'nvim-treesitter/nvim-treesitter',
        'nvim-tree/nvim-web-devicons'
      },
      keys = {
        { '<A-d>', '<cmd>Lspsaga term_toggle<CR>', mode = { 'n', 't' }}
      },
      config = true
    },
    {
      'williamboman/mason.nvim',
      lazy = false,
      opts = {
        ui = {
          icons = {
            package_installed = '✓',
            package_pending = '➜',
            package_uninstalled = '✗'
          }
        },
        max_concurrent_installers = 12
      }
    },
    {
      'williamboman/mason-lspconfig.nvim',
      dependencies = 'hrsh7th/cmp-nvim-lsp',
      opts = {
        ensure_installed = {
          'asm_lsp', 'bashls', 'clangd', 'clojure_lsp',
          'dockerls', 'docker_compose_language_service', 'gradle_ls', 'jsonls',
          'hls', 'html', 'jdtls', 'tsserver',
          'kotlin_language_server', 'lua_ls', 'autotools_ls', 'marksman',
          'pyright', 'rust_analyzer', 'sqls', 'taplo',
          'lemminx', 'vimls', 'yamlls', 'zls'
        },
        handlers =  {
          function(lang_server)
            require 'lspconfig'[lang_server].setup {
              capabilities = require 'cmp_nvim_lsp'.default_capabilities()
            }
          end,

          clangd = function()
            require 'lspconfig'.clangd.setup {
              capabilities = require 'cmp_nvim_lsp'.default_capabilities(),
              cmd = { 'clangd', '--offset-encoding=utf-16' }
            }
          end,

          lua_ls = function()
            require 'lspconfig'.lua_ls.setup {
              capabilities = require 'cmp_nvim_lsp'.default_capabilities(),
              settings = { Lua = { diagnostics = { globals = { 'vim' }}}}
            }
          end,

          tsserver = function ()
            require 'lspconfig'.ts_ls.setup {
              capabilities = require 'cmp_nvim_lsp'.default_capabilities()
            }
          end
        }
      }
    },
    {
      'hrsh7th/nvim-cmp',
      dependencies = {
        { 'onsails/lspkind.nvim' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-cmdline' },
        { 'hrsh7th/cmp-buffer' },
        { 'hrsh7th/cmp-path' },
        { 'hrsh7th/cmp-vsnip' },
        { 'hrsh7th/vim-vsnip' },
      },
      config = function()
        local cmp = require 'cmp'
        cmp.setup {
          snippet = { expand = function(args) vim.fn['vsnip#anonymous'](args.body) end },
          mapping = cmp.mapping.preset.insert {
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<Up>'] = cmp.mapping.select_prev_item { behavior = 'select' },
            ['<Down>'] = cmp.mapping.select_next_item { behavior = 'select' },
            ['<CR>'] = cmp.mapping.confirm { select = false }
          },
          sources = cmp.config.sources(
            {{ name = 'nvim_lsp' }, { name = 'vsnip' }},
            {{ name = 'buffer' }}),
          formatting = {
            format = require 'lspkind'.cmp_format {
              mode = 'symbol_text',
              maxwidth = 50,
              ellipsis_char = '...',
              show_labelDetails = true,
              before = function(_, item) return item end
            }
          }
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
    },
    config = function()
      vim.diagnostic.config {
        float = {
          focusable = false,
          style = 'minimal',
          border = 'rounded',
          source = 'always',
          header = '',
          prefix = ''
        }
      }
    end
  }
}
