return {
  {
    'dracula/vim',
    name = 'dracula',
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'dracula'
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons'
    },
    opts = {
      options = {
        icons_enabled = true,
        theme = 'dracula',
        component_separators = { left = '|', right = '|' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
          statusline = 1000,
          tabline = 1000,
          winbar = 1000
        }
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { 'filename', 'filesize', 'searchcount' },
        lualine_x = { 'selectioncount', 'encoding', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
      },
      tabline = {},
      winbar = {},
      inactive_winbar = {},
      extensions = {}
    }
  }
}
