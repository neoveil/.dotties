local set = vim.keymap.set
vim.g.mapleader = ' '

set('n', '<leader>fe', vim.cmd.Ex)
set('v', 'J', ':m \'>+1<CR>gv=gv')
set('v', 'K', ':m \'<-2<CR>gv=gv')

set('n', '<Esc>', '<cmd>noh<CR>')
set('n', 'n', 'nzzzv')
set('n', 'N', 'Nzzzv')
set('n', '<leader>s', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

local _search = function(input)
  local back3 = vim.api.nvim_replace_termcodes('<Left><Left><Left>', true, true, true)
  vim.api.nvim_feedkeys(':' .. string.format('%%s/%s//gI', input) .. back3, 'c', false)
end

local search_word = function()
  local word = vim.fn.input 'Word > '
  _search('\\<' .. word .. '\\>')
end
set('n', '<leader>sw', search_word)

local search_regex = function()
  local regex = vim.fn.input 'Regex > '
  _search(regex)
end
set('n', '<leader>sr', search_regex)

set('x', '<leader>p', [["_dP]])
set({ 'n', 'v' }, '<leader>y', [["+y]])
set('n', '<leader>Y', [["+Y]])
set({ 'n', 'v' }, '<leader>d', [["_d]])

set('n', '<C-d>', '<C-d>zz')
set('n', '<C-u>', '<C-u>zz')
set({ 'n', 'v' }, '<Up>', '<Up>zz')
set({ 'n', 'v' }, '<Down>', '<Down>zz')
set({ 'n', 'v'}, 'k', 'kzz')
set({ 'n', 'v'}, 'j', 'jzz')
set('i', '<C-c>', '<Esc>')
set('n', 'Q', '<nop>')
set('n', '<leader>f', vim.lsp.buf.format)
