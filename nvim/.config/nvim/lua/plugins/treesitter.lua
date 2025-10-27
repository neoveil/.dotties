return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  main = 'nvim-treesitter.configs',
  opts = {
    ensure_installed = {
      'asm', 'bash', 'dockerfile', 'git_config', 'git_rebase',
      'gitattributes', 'gitcommit', 'gitignore', 'ini', 'javascript',
      'json', 'lua', 'make', 'markdown', 'markdown_inline', 'nasm',
      'objdump', 'properties', 'proto', 'ssh_config', 'toml', 'vim',
      'vimdoc', 'xml', 'yaml'
    },
    sync_install = false,
    auto_install = true,
    indent = { enable = true },
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = { 'markdown' }
    }
  }
}
