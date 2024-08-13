return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  main = 'nvim-treesitter.configs',
  opts = {
    ensure_installed = {
      'asm', 'bash', 'c', 'clojure', 'cpp',
      'css', 'dockerfile', 'git_config', 'git_rebase', 'gitattributes',
      'gitcommit', 'gitignore', 'haskell', 'html', 'http',
      'ini', 'java', 'javascript', 'json', 'kotlin',
      'lua', 'make', 'markdown', 'markdown_inline', 'nasm',
      'objdump', 'properties', 'proto', 'python', 'rust',
      'sql', 'ssh_config', 'toml', 'tsx', 'typescript',
      'vim', 'vimdoc', 'xml', 'yaml', 'zig'
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
