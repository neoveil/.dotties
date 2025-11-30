local set = vim.keymap.set
vim.g.mapleader = ' ' -- space as leader

set('n', '<leader>fe', vim.cmd.Ex) -- open file explorer
set('v', 'J', ':m \'>+1<CR>gv=gv') -- move line down
set('v', 'K', ':m \'<-2<CR>gv=gv') -- move line up

set('n', '<Esc>', '<cmd>noh<CR>') -- disable search highlight on ESC
set('n', 'n', 'nzzzv') -- next search match
set('n', 'N', 'Nzzzv') -- previous search match
set('n', '<leader>s', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]]) -- replace globally word under cursor

local _search = function(input)
  local back3 = vim.api.nvim_replace_termcodes('<Left><Left><Left>', true, true, true)
  vim.api.nvim_feedkeys(':' .. string.format('%%s/%s//gI', input) .. back3, 'c', false)
end

local search_word = function()
  local word = vim.fn.input 'Word > '
  _search('\\<' .. word .. '\\>')
end
set('n', '<leader>sw', search_word) -- prompt for a word and perform a replace

local search_regex = function()
  local regex = vim.fn.input 'Regex > '
  _search(regex)
end
set('n', '<leader>sr', search_regex) -- prompt for a regex and perform a replace

set('x', '<leader>p', [["_dP]]) -- replace selection with clipboard content (without losing clipboard content)
set({ 'n', 'v' }, '<leader>y', [["+y]]) -- yank selection (or line/word with 'y'/'w')
set({ 'n', 'v' }, '<leader>d', [["_d]]) -- delete selection (or line/word with 'd'/'w') to the black hole register

-- cursor movement to beginning/end of line
set({ 'n', 'v' }, '<C-e>', '$')
set({ 'n', 'v' }, '<C-a>', '0')
set('i', '<C-e>', '<END>')
set('i', '<C-a>', '<HOME>')

-- scroll up/down and recenter
set('n', '<C-d>', '<C-d>zz')
set('n', '<C-u>', '<C-u>zz')

set('i', '<C-c>', '<Esc>') -- C-c should be ESC
set('n', 'Q', '<nop>') -- yeah...
