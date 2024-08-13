require 'options'
require 'keymaps'
require 'lazy-setup'

-- When we open a file, it should be nice to be at the file's directory, right?
vim.api.nvim_create_autocmd('BufEnter', {
  pattern = '*',
  callback = function() vim.cmd('silent! lcd ' .. vim.fn.expand '%:p:h') end
})
