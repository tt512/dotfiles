-- vim: set et ts=2 sw=2 sts=2 fdm=marker:
-- Bootstrap {{{1
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.system { 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path }
  vim.cmd [[packadd packer.nvim]]
end
-- }}}
-- Packages {{{1
require('packer').startup {
  function(use, use_rocks)
    use 'wbthomason/packer.nvim'

    -- LSP
    use 'neovim/nvim-lspconfig'
    use 'williamboman/nvim-lsp-installer'
    use 'tami5/lspsaga.nvim'
    use { 'jose-elias-alvarez/null-ls.nvim', requires = { 'nvim-lua/plenary.nvim' } }
    use 'ray-x/lsp_signature.nvim'
    use 'folke/trouble.nvim'
    use 'folke/lua-dev.nvim'

    -- Completion
    use 'windwp/nvim-autopairs'
    use {
      'hrsh7th/nvim-cmp',
      requires = {
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-nvim-lua',
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-vsnip',
        'hrsh7th/vim-vsnip',
      },
    }

    -- Treesitter
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    -- UI
    use { 'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim' } }
    use {
      'nvim-neo-tree/neo-tree.nvim',
      requires = { 'nvim-lua/plenary.nvim', 'kyazdani42/nvim-web-devicons', 'MunifTanjim/nui.nvim' },
      branch = 'v2.x',
    }
    use 'liuchengxu/vista.vim'
    use { 'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons', opt = true } }
    use 'akinsho/toggleterm.nvim'
    use 'petertriho/nvim-scrollbar'

    -- Debugging
    use { 'rcarriga/vim-ultest', requires = { 'vim-test/vim-test' }, run = ':UpdateRemotePlugins' }
    use { 'rcarriga/nvim-dap-ui', requires = { 'mfussenegger/nvim-dap' } }

    -- Utility
    use 'famiu/bufdelete.nvim'
    use 'lukas-reineke/indent-blankline.nvim'
    use 'tpope/vim-sleuth'
    use 'ojroques/vim-oscyank'
    use 'vim-skk/eskk.vim'
    use 'ludovicchabant/vim-gutentags'
    use 'rafcamlet/nvim-luapad'
    use 'ap/vim-css-color'
    use {
      'norcalli/nvim-colorizer.lua',
      config = function()
        require('colorizer').setup()
      end,
    }

    -- Git
    use 'lambdalisue/gina.vim'
    use { 'ruifm/gitlinker.nvim', requires = 'nvim-lua/plenary.nvim' }
    use { 'lewis6991/gitsigns.nvim', requires = 'nvim-lua/plenary.nvim' }

    -- Colorscheme
    use 'folke/tokyonight.nvim'
  end,
  config = { display = { open_fn = require('packer.util').float } },
}
-- }}}1
-- General Settings {{{1
vim.opt.virtualedit = 'all'
vim.opt.mouse = 'a'
vim.opt.cursorline = true
vim.opt.undofile = true
vim.opt.smartindent = true

vim.opt.wildmode = { 'longest:full', 'full' }
vim.opt.wildignore = '*.o'
vim.opt.wildignorecase = true
vim.opt.hidden = true

vim.opt.termguicolors = true

vim.opt.clipboard:append 'unnamedplus'
vim.opt.signcolumn = 'yes'
vim.opt.relativenumber = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4

vim.cmd [[
augroup DefaultIndentPerFile
  autocmd!
  autocmd FileType lua,html,javascript,typescript,pascal,ruby,yaml,zsh,cucumber setl et ts=2 sw=2 sts=2
  autocmd FileType conf,xf86conf setl noet ts=8 sw=8 sts=8
  autocmd FileType go setl noet ts=4 sw=4 sts=4
augroup END
]]

vim.cmd [[
augroup CursorRestore
  autocmd!
  autocmd BufReadPost * if line("'\"") > 0 && line ("'\"") <= line("$") | exe "normal! g'\"" | endif
augroup END
]]

vim.cmd [[
augroup FoldRestore
  autocmd!
  autocmd BufWinLeave *.* mkview
  autocmd BufWinEnter *.* silent! loadview
augroup END
]]

vim.cmd [[colorscheme tokyonight]]
-- }}}1
-- LSP {{{1
local lsp_installer = require 'nvim-lsp-installer'

local function on_attach(client, bufnr)
  -- Set up buffer-local keymaps (vim.api.nvim_buf_set_keymap()), etc.
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', { noremap = true, silent = true })
end

local enhance_server_opts = {
  -- Provide settings that should only apply to the "eslintls" server
  ['sumneko_lua'] = function(opts)
    opts.settings = {
      Lua = {
        workspace = {
          checkThirdParty = false,
        },
      },
    }
    opts.on_attach = function(client, bufnr)
      on_attach(client, bufnr)
      client.resolved_capabilities.document_formatting = false
      client.resolved_capabilities.document_range_formatting = false
    end
    return require('lua-dev').setup { lspconfig = opts }
  end,
}

lsp_installer.on_server_ready(function(server)
  -- Specify the default options which we'll use to setup all servers
  local opts = {
    on_attach = on_attach,
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
  }

  if enhance_server_opts[server.name] then
    -- Enhance the default opts with the server-specific ones
    opts = enhance_server_opts[server.name](opts)
  end

  server:setup(opts)
end)

local null_ls = require 'null-ls'
null_ls.setup {
  -- you can reuse a shared lspconfig on_attach callback here
  on_attach = function(client)
    if client.resolved_capabilities.document_formatting then
      vim.cmd [[
        augroup LspFormatting
            autocmd! * <buffer>
            autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
        augroup END
        ]]
    end
  end,
  sources = {
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.formatting.rustfmt,
    null_ls.builtins.formatting.gofmt,
    null_ls.builtins.formatting.clang_format,
  },
}

require('lspsaga').setup()
require('lsp_signature').setup()
require('trouble').setup()
-- }}}1
-- Completion {{{1
vim.opt.completeopt = { 'menuone', 'noselect' }

local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn['vsnip#anonymous'](args.body)
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm { select = true },
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'nvim_lua' },
  }, {
    { name = 'path' },
    { name = 'buffer' },
  }),
}

require('cmp').setup.cmdline(':', {
  sources = {
    { name = 'cmdline' },
  },
})

require('cmp').setup.cmdline('/', {
  sources = {
    { name = 'buffer' },
  },
})

require('nvim-autopairs').setup {}
-- If you want insert `(` after select function or method item
local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done { map_char = { tex = '' } })
-- }}}1
-- Treesitter {{{1
require('nvim-treesitter.configs').setup { ensure_installed = 'maintained', highlight = { enable = true } }
-- }}}1
-- UI {{{1
-- Lualine {{{2
local lualine_neotree = {
  sections = {
    lualine_a = { vim.deepcopy(require('lualine.extensions.nerdtree').sections) },
  },
  filetypes = { 'neo-tree' },
}

local lualine_vista = {
  sections = {
    lualine_a = {
      function()
        return vim.g.vista.provider
      end,
    },
  },
  filetypes = { 'vista' },
}

local function lualine_eskk()
  if vim.fn.mode() == 'i' and vim.g.loaded_eskk and vim.fn['eskk#is_enabled']() == 1 then
    local mode = vim.fn['eskk#get_mode']()
    return vim.g['eskk#statusline_mode_strings'][mode] or '??'
  else
    return ''
  end
end

require('lualine').setup {
  options = { theme = 'tokyonight', section_separators = '', component_separators = '│' },
  sections = {
    lualine_a = {
      {
        'mode',
        fmt = function(str)
          return str:sub(1, 1)
        end,
      },
    },
    lualine_b = { 'branch' },
    lualine_c = { 'diagnostics', 'filename', 'b:vista_nearest_method_or_function' },
    lualine_x = { 'encoding', 'fileformat', 'filetype', lualine_eskk },
    lualine_y = { 'progress' },
    lualine_z = { 'location' },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { 'filename' },
    lualine_x = { 'location' },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  extensions = { lualine_neotree, 'toggleterm', 'quickfix', lualine_vista },
}
-- }}}2
-- Telescope {{{2
local actions = require 'telescope.actions'
require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical',
    mappings = {
      i = {
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
        ['<esc>'] = actions.close,
      },
    },
  },
  pickers = { buffers = { sort_lastused = true } },
}
-- }}}2
-- toggleterm.nvim {{{2
require('toggleterm').setup()
-- }}}2
-- vista {{{2
vim.g.vista_default_executive = 'nvim_lsp'
-- }}}2
-- nvim-scrollbar {{{2
local colors = require('tokyonight.colors').setup()
require('scrollbar').setup {
  handle = {
    color = colors.blue0,
  },
}
-- }}}2
-- neo-tree.nvim {{{2
vim.cmd [[
hi link NeoTreeNormal NvimTreeNormal
hi link NeoTreeNormalNC NvimTreeNormalNC
hi link NeoTreeIndentMarker NvimTreeIndentMarker
]]
require('neo-tree').setup {
  close_if_last_window = true,
  enable_git_status = true,
  enable_diagnostics = false,
  filesystem = {
    use_libuv_file_watcher = true,
    window = {
      width = 30,
    },
  },
}
-- }}}2
-- }}}1
-- Debugging {{{1
vim.api.nvim_set_keymap('n', '<F5>', [[<cmd>lua require'dap'.continue()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<F10>', [[<cmd>lua require'dap'.step_over()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<F11>', [[<cmd>lua require'dap'.step_into()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<F12>', [[<cmd>lua require'dap'.step_out()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap(
  'n',
  '<leader>b',
  [[<cmd>lua require'dap'.toggle_breakpoint()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<leader>B',
  [[<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<leader>lp',
  [[<cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap('n', '<leader>dr', [[<cmd>lua require'dap'.repl.open()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>dl', [[<cmd>lua require'dap'.run_last()<CR>]], { noremap = true, silent = true })

local dap_install = require 'dap-install'
local dbg_list = require('dap-install.api.debuggers').get_installed_debuggers()

for _, debugger in ipairs(dbg_list) do
  dap_install.config(debugger)
end

local dap = require 'dap'
dap.adapters.lldb = {
  type = 'executable',
  command = '/usr/bin/lldb-vscode', -- adjust as needed
  name = 'lldb',
}
dap.configurations.cpp = {
  {
    name = 'Launch',
    type = 'lldb',
    request = 'launch',
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},

    -- 💀
    -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
    --
    --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
    --
    -- Otherwise you might get the following error:
    --
    --    Error on launch: Failed to attach to the target process
    --
    -- But you should be aware of the implications:
    -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html

    runInTerminal = false,

    -- 💀
    -- If you use `runInTerminal = true` and resize the terminal window,
    -- lldb-vscode will receive a `SIGWINCH` signal which can cause problems
    -- To avoid that uncomment the following option
    -- See https://github.com/mfussenegger/nvim-dap/issues/236#issuecomment-1066306073
    postRunCommands = { 'process handle -p true -s false -n false SIGWINCH' },
  },
}

-- If you want to use this for rust and c, add something like this:

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

require('dapui').setup()
local dapui = require 'dapui'
dap.listeners.after.event_initialized['dapui_config'] = function()
  dapui.open()
end
dap.listeners.before.event_terminated['dapui_config'] = function()
  dapui.close()
end
dap.listeners.before.event_exited['dapui_config'] = function()
  dapui.close()
end

require('ultest').setup {
  builders = {
    ['go#gotest'] = function(cmd)
      local args = {}
      for i = 3, #cmd - 1, 1 do
        local arg = cmd[i]
        if vim.startswith(arg, '-') then
          -- Delve requires test flags be prefix with 'test.'
          arg = '-test.' .. string.sub(arg, 2)
        end
        args[#args + 1] = arg
      end
      return {
        dap = {
          type = 'go',
          request = 'launch',
          mode = 'test',
          program = '${workspaceFolder}',
          dlvToolPath = vim.fn.exepath 'dlv',
          args = args,
        },
        parse_result = function(lines)
          return lines[#lines] == 'FAIL' and 1 or 0
        end,
      }
    end,
  },
}
-- }}}1
-- Utility {{{1
-- OSCYank
vim.cmd [[
augroup OSCYank
  autocmd!
  autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | OSCYankReg " | endif
augroup END
]]

-- eskk
vim.g['eskk#large_dictionary'] = '~/SKK-JISYO.XL'

require('indent_blankline').setup {
  filetype_exclude = { 'help', 'neo-tree', 'vista' },
  use_treesitter = true,
}
-- }}}1
-- Git {{{1
require('gitlinker').setup {
  opts = {
    action_callback = function(url)
      vim.fn.setreg('+', url)
      local backup = vim.g.oscyank_silent
      vim.g.oscyank_silent = true
      vim.cmd [[OSCYankReg +]]
      vim.g.oscyank_silent = backup
    end,
  },
}
require('gitsigns').setup()
-- }}}1
-- Global Keymappings {{{1
vim.api.nvim_set_keymap('n', 'gr', ':<C-u>Telescope lsp_references<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gd', ':<C-u>Telescope lsp_definitions<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gi', ':<C-u>Telescope lsp_implementations<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'ga', ':<C-u>Lspsaga code_action<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gn', ':<C-u>Lspsaga rename<cr>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<space>b', ':<C-u>Telescope buffers<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>f', ':<C-u>Telescope find_files<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>r', ':<C-u>Telescope oldfiles<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>o', ':<C-u>Telescope treesitter<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>g', ':<C-u>Telescope live_grep<cr>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<space>m', ':<C-u>Vista<cr>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<space>t', ':<C-u>NeoTreeShowToggle<cr>', { noremap = true, silent = true })

vim.api.nvim_set_keymap(
  'n',
  '<space>R',
  ':<C-u>so $MYVIMRC<cr>:echo "reloaded vimrc."<cr>',
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap('n', '<space>E', ':<C-u>e $MYVIMRC<cr>', { noremap = true, silent = true })
-- }}}1
