return {
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  branch = 'main',
  build = ':TSUpdate',
  config = function()
    require 'nvim-treesitter'.install {
      'asm', 'awk', 'bash', 'c', 'caddy', 'clojure',
      'cmake', 'cpp', 'css', 'desktop', 'dockerfile',
      'git_config', 'git_rebase', 'gitattributes',
      'gitcommit', 'gitignore', 'go', 'gomod', 'gosum',
      'gowork', 'html', 'hyprlang', 'ini', 'java', 'javadoc',
      'javascript', 'jq', 'json', 'kitty', 'kotlin', 'udev',
      'linkerscript', 'llvm', 'lua', 'make', 'markdown',
      'markdown_inline', 'meson', 'nasm', 'nginx', 'ninja',
      'objdump', 'properties', 'proto', 'python', 'rasi',
      'rust', 'sql', 'ssh_config', 'toml', 'tsx', 'typescript',
      'vim', 'xml', 'yaml', 'zathurarc', 'zig', 'zsh'
    }

    vim.api.nvim_create_autocmd('FileType', {
        pattern = { '<filetype>' },
        callback = function()
          vim.treesitter.start()
          vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end
    })
  end
}
