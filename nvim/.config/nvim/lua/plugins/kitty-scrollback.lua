return {
  'mikesmithgh/kitty-scrollback.nvim',
  enabled = true,
  lazy = true,
  cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth' },
  event = { 'User KittyScrollbackLaunch' },
  version = '^5.0.0',
  main = 'kitty-scrollback',
  opts = {
    original_config = { restore_options = true }
  },
  config = true
}
