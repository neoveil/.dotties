return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = {
    { 'nvim-lua/plenary.nvim' },
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' }
  },
  keys = {
    { '<leader>pf', function() require('telescope.builtin').find_files() end },
    { '<leader>ps', function() require('telescope.builtin').grep_string { search = vim.fn.input('GREP > ') } end },
    { '<leader>vh', function() require('telescope.builtin').help_tags() end }
  },
  config = function()
    local ts = require 'telescope'
    ts.setup {}
    ts.load_extension 'fzf'
  end
}
