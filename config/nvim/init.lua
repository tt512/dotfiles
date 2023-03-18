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
  function(use)
    use 'wbthomason/packer.nvim'

    -- LSP
    use 'neovim/nvim-lspconfig'
    use { 'williamboman/mason.nvim' }
    use { 'williamboman/mason-lspconfig.nvim' }
    use { 'jose-elias-alvarez/null-ls.nvim', requires = { 'nvim-lua/plenary.nvim' } }
    use 'folke/neodev.nvim'
    use 'j-hui/fidget.nvim'
    use 'simrat39/rust-tools.nvim'
    use {
      'folke/trouble.nvim',
      requires = 'kyazdani42/nvim-web-devicons',
      config = function()
        require('trouble').setup {
          mode = 'document_diagnostics',
        }
      end,
    }
    use {
      'kosayoda/nvim-lightbulb',
      config = function()
        require('nvim-lightbulb').setup { autocmd = { enabled = true } }
      end,
    }

    -- Completion
    use {
      'hrsh7th/nvim-cmp',
      requires = {
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-nvim-lsp-signature-help',
        'hrsh7th/cmp-nvim-lua',
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-cmdline',
        'dcampos/nvim-snippy',
        'dcampos/cmp-snippy',
      },
    }
    use 'windwp/nvim-autopairs'
    use 'windwp/nvim-ts-autotag'

    -- Treesitter
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

    -- UI
    use { 'nvim-lualine/lualine.nvim', requires = { 'kyazdani42/nvim-web-devicons' } }
    use { 'akinsho/bufferline.nvim', tag = 'v2.*', requires = 'kyazdani42/nvim-web-devicons' }
    use {
      'utilyre/barbecue.nvim',
      tag = '*',
      requires = {
        'SmiteshP/nvim-navic',
        'nvim-tree/nvim-web-devicons', -- optional dependency
      },
      after = 'nvim-web-devicons', -- keep this if you're using NvChad
      config = function()
        require('barbecue').setup {
          attach_navic = false,
          show_dirname = false,
          show_basename = false,
          show_modified = false,
          symbols = {
            separator = '',
          },
          kinds = {
            File = '',
            Module = '',
            Namespace = '',
            Package = '',
            Class = '',
            Method = '',
            Property = '',
            Field = 'פּ',
            Constructor = '',
            Enum = '',
            Interface = '',
            Function = '',
            Variable = '',
            Constant = '',
            String = '',
            Number = '',
            Boolean = '',
            Array = '',
            Object = '⦿',
            Key = '',
            Null = 'NULL',
            EnumMember = '',
            Struct = '',
            Event = '⚡',
            Operator = '',
            TypeParameter = '',
          },
        }
      end,
    }
    use 'petertriho/nvim-scrollbar'
    use 'stevearc/stickybuf.nvim'
    use {
      'goolord/alpha-nvim',
      requires = { 'kyazdani42/nvim-web-devicons' },
      config = function()
        require('alpha').setup(require('alpha.themes.startify').config)
      end,
    }
    use {
      'nvim-telescope/telescope.nvim',
      branch = '0.1.x',
      requires = { 'nvim-lua/plenary.nvim' },
    }
    use {
      'nvim-neo-tree/neo-tree.nvim',
      requires = { 'nvim-lua/plenary.nvim', 'kyazdani42/nvim-web-devicons', 'MunifTanjim/nui.nvim' },
      branch = 'v2.x',
    }
    use { 'akinsho/toggleterm.nvim', tag = '*' }
    use 'simrat39/symbols-outline.nvim'
    use 'liuchengxu/vista.vim'
    use 'stevearc/dressing.nvim'
    use {
      'rcarriga/nvim-notify',
      config = function()
        vim.notify = require 'notify'
      end,
    }
    use {
      'luukvbaal/stabilize.nvim',
      config = function()
        require('stabilize').setup()
      end,
    }

    -- Debugging
    use {
      'nvim-neotest/neotest',
      requires = { 'nvim-lua/plenary.nvim', 'nvim-treesitter/nvim-treesitter' },
    }
    use 'nvim-neotest/neotest-python'

    use { 'rcarriga/nvim-dap-ui', requires = { 'mfussenegger/nvim-dap' } }
    use 'mfussenegger/nvim-dap-python'

    -- Utility
    use 'famiu/bufdelete.nvim'
    use 'lukas-reineke/indent-blankline.nvim'
    use 'tpope/vim-sleuth'
    use 'machakann/vim-sandwich'
    use 'ojroques/vim-oscyank'
    use 'vim-skk/eskk.vim'
    use { 'rafcamlet/nvim-luapad', cmd = { 'Luapad', 'LuaRun' } }
    use 'rgroli/other.nvim'
    use 'norcalli/nvim-colorizer.lua'
    use 'ggandor/lightspeed.nvim'
    use 'folke/which-key.nvim'
    use 'ludovicchabant/vim-gutentags'

    -- Git
    use { 'lambdalisue/gina.vim', cmd = { 'Gina' } }
    use {
      'ruifm/gitlinker.nvim',
      requires = 'nvim-lua/plenary.nvim',
    }
    use { 'lewis6991/gitsigns.nvim', requires = 'nvim-lua/plenary.nvim' }

    -- Colorscheme
    use 'EdenEast/nightfox.nvim'
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

vim.opt.clipboard:append 'unnamedplus'
vim.opt.signcolumn = 'yes'
vim.opt.relativenumber = true

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4

vim.opt.termguicolors = true

vim.cmd [[colorscheme nordfox]]

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

local signs = { Error = ' ', Warn = ' ', Hint = ' ', Info = ' ' }
for type, icon in pairs(signs) do
  local hl = 'DiagnosticSign' .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = '' })
end
-- }}}1
-- LSP {{{1
require('neodev').setup {}
require('mason').setup {}
require('mason-lspconfig').setup {}
local lspconfig = require 'lspconfig'

local function on_attach(client, bufnr)
  -- Set up buffer-local keymaps (vim.api.nvim_buf_set_keymap()), etc.
  local wk = require 'which-key'

  wk.register({
    g = {
      D = { vim.lsp.buf.declaration, 'Go to declaration' },
      d = { vim.lsp.buf.definition, 'Go to definition' },
      i = { vim.lsp.buf.implementation, 'Go to implementation' },
      a = { vim.lsp.buf.code_action, 'Show code action' },
      n = { vim.lsp.buf.rename, 'Rename' },
      r = { vim.lsp.buf.references, 'Show references' },
      z = { vim.diagnostic.open_float, 'Show line diagnostics' },
    },
    K = { vim.lsp.buf.hover, 'Show hover doc' },
    ['<C-k>'] = { vim.lsp.buf.signature_help, 'Show signature help' },
  }, { buffer = bufnr })

  if client.server_capabilities['documentSymbolProvider'] then
    require('nvim-navic').attach(client, bufnr)
  end

  client.server_capabilities.documentFormattingProvider = false
  client.server_capabilities.documentRangeFormattingProvider = false
end

require('mason-lspconfig').setup_handlers {
  -- The first entry (without a key) will be the default handler
  -- and will be called for each installed server that doesn't have
  -- a dedicated handler.
  function(server_name) -- default handler (optional)
    require('lspconfig')[server_name].setup {
      on_attach = on_attach,
    }
  end,
  -- Next, you can provide targeted overrides for specific servers.
  ['rust_analyzer'] = function()
    require('rust-tools').setup {
      server = {
        on_attach = on_attach,
      },
    }
  end,
  ['sumneko_lua'] = function()
    lspconfig.sumneko_lua.setup {
      lspconfig = {
        settings = {
          Lua = {
            completion = {
              callSnippet = 'Replace',
            },
          },
        },
      },
      on_attach = on_attach,
    }
  end,
}

local augroup = vim.api.nvim_create_augroup('LspFormatting', { clear = true })
local null_ls = require 'null-ls'
null_ls.setup {
  -- you can reuse a shared lspconfig on_attach callback here
  on_attach = function(client, bufnr)
    if client.supports_method 'textDocument/formatting' then
      vim.api.nvim_create_autocmd('BufWritePre', {
        group = augroup,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format { bufnr = bufnr }
        end,
      })
    end
  end,
  sources = {
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.formatting.gofmt,
    null_ls.builtins.formatting.clang_format,
    null_ls.builtins.formatting.autopep8,
    null_ls.builtins.formatting.terraform_fmt,
    null_ls.builtins.formatting.prettierd,
    null_ls.builtins.diagnostics.eslint_d.with {
      diagnostics_postprocess = function(diagnostic)
        diagnostic.severity = vim.diagnostic.severity['WARN']
      end,
    },
    null_ls.builtins.code_actions.eslint_d,
  },
}

require('fidget').setup()
-- }}}1
-- Completion {{{1
vim.opt.completeopt = { 'menuone', 'noselect' }

local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body)
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm { select = true },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ['<C-n>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ['<C-p>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'snippy' },
    { name = 'nvim_lua' },
  }, {
    { name = 'path' },
    { name = 'buffer' },
  }),
}

require('cmp').setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'cmdline' },
  },
})

require('cmp').setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
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
require('nvim-treesitter.configs').setup {
  highlight = { enable = true },
  autotag = { enable = true },
}
-- }}}1
-- UI {{{1
-- Lualine {{{2
local function lualine_eskk()
  if vim.fn.mode() == 'i' and vim.g.loaded_eskk and vim.fn['eskk#is_enabled']() == 1 then
    local mode = vim.fn['eskk#get_mode']()
    return vim.g['eskk#statusline_mode_strings'][mode] or '??'
  else
    return ''
  end
end

require('lualine').setup {
  options = { theme = 'nordfox', section_separators = '', component_separators = '│', globalstatus = true },
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
    lualine_c = { 'diagnostics', 'filename' },
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
  extensions = { 'neo-tree', 'toggleterm', 'quickfix', 'symbols-outline' },
}
-- }}}2
-- bufferline.nvim {{{2
require('bufferline').setup {}
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
require('scrollbar').setup {
  excluded_filetypes = { 'neo-tree', 'Outline' },
}
-- }}}2
-- neo-tree.nvim {{{2
require('neo-tree').setup {
  close_if_last_window = true,
  enable_git_status = true,
  enable_diagnostics = false,
  filesystem = {
    use_libuv_file_watcher = true,
    window = {
      width = 35,
    },
  },
}
-- }}}2
-- symbols-outline {{{2
require('symbols-outline').setup {
  auto_preview = false,
  relative_width = false,
  width = 35,
  symbol_blacklist = { 'String', 'Number', 'Variable' },
  symbols = {
    File = { icon = '', hl = 'TSURI' },
    Module = { icon = '', hl = 'TSNamespace' },
    Namespace = { icon = '', hl = 'TSNamespace' },
    Package = { icon = '', hl = 'TSNamespace' },
    Class = { icon = '', hl = 'TSType' },
    Method = { icon = '', hl = 'TSMethod' },
    Property = { icon = '', hl = 'TSMethod' },
    Field = { icon = 'פּ', hl = 'TSField' },
    Constructor = { icon = '', hl = 'TSConstructor' },
    Enum = { icon = '', hl = 'TSType' },
    Interface = { icon = '', hl = 'TSType' },
    Function = { icon = '', hl = 'TSFunction' },
    Variable = { icon = '', hl = 'TSConstant' },
    Constant = { icon = '', hl = 'TSConstant' },
    String = { icon = '', hl = 'TSString' },
    Number = { icon = '', hl = 'TSNumber' },
    Boolean = { icon = '', hl = 'TSBoolean' },
    Array = { icon = '', hl = 'TSConstant' },
    Object = { icon = '⦿', hl = 'TSType' },
    Key = { icon = '', hl = 'TSType' },
    Null = { icon = 'NULL', hl = 'TSType' },
    EnumMember = { icon = '', hl = 'TSField' },
    Struct = { icon = '', hl = 'TSType' },
    Event = { icon = '⚡', hl = 'TSType' },
    Operator = { icon = '', hl = 'TSOperator' },
    TypeParameter = { icon = '', hl = 'TSParameter' },
  },
}
-- }}}2
-- stickybuf {{{2
require('stickybuf').setup {
  get_auto_pin = function(bufnr)
    local pin_filetypes = {
      'Outline',
      'toggleterm',
    }
    local filetype = vim.bo[bufnr].filetype
    for _, t in pairs(pin_filetypes) do
      if filetype == t then
        return 'filetype'
      end
    end

    return require('stickybuf').should_auto_pin(bufnr)
  end,
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

require('neotest').setup {
  adapters = {
    require 'neotest-python',
  },
}
-- }}}1
-- Utility {{{1
-- OSCYank
vim.cmd [[
augroup OSCYank
  autocmd!
  autocmd TextYankPost *
    \ if v:event.operator is 'y' && v:event.regname is '+' |
    \ execute 'OSCYankRegister +' |
    \ endif
augroup END
]]

-- eskk
vim.g['eskk#large_dictionary'] = '~/SKK-JISYO.XL'

require('indent_blankline').setup {
  filetype_exclude = { 'help', 'neo-tree', 'vista', 'Outline' },
  use_treesitter = true,
}

-- other.nvim
require('other-nvim').setup {
  mappings = {
    {
      pattern = '/src/(.*)/(.*).tsx$',
      target = '/src/%1/%2.stories.tsx$',
    },
  },
}

-- nvim-colorizer
require('colorizer').setup {
  '*',
  '!packer',
}

-- which-key
require('which-key').setup()

-- gutentags
vim.g.gutentags_enabled = false
-- }}}1
-- Git {{{1
require('gitlinker').setup {
  opts = {
    action_callback = function(url)
      vim.fn.setreg('+', url)
      local backup = vim.g.oscyank_silent
      vim.g.oscyank_silent = true
      vim.cmd [[OSCYankRegister +]]
      vim.g.oscyank_silent = backup
    end,
  },
}
require('gitsigns').setup()
-- }}}1
-- Global Keymappings {{{1
local function reload_config()
  local hls_status = vim.v.hlsearch
  for name, _ in pairs(package.loaded) do
    if name:match '^cnull' then
      package.loaded[name] = nil
    end
  end

  dofile(vim.env.MYVIMRC)
  if hls_status == 0 then
    vim.opt.hlsearch = false
  end
end

require('which-key').register({
  f = {
    name = 'Finder',
    f = { '<Cmd>Telescope find_files<cr>', 'Find files' },
    r = { '<Cmd>Telescope oldfiles<cr>', 'MRU Files' },
    b = { '<Cmd>Telescope buffers<cr>', 'Buffers' },
    g = { '<Cmd>Telescope live_grep<cr>', 'Grep files' },
  },
  o = {
    name = 'Open/Close Windows',
    f = { '<Cmd>Neotree toggle<cr>', 'Toggle file tree' },
    s = { '<Cmd>SymbolsOutline<cr>', 'Toggle symbol tree' },
    d = { '<Cmd>TroubleToggle<cr>', 'Toggle diagnostics list' },
    t = { '<Cmd>ToggleTerm<cr>', 'Toggle terminal' },
  },
  ['E'] = { ':<C-u>e $MYVIMRC<cr>', 'Open config' },
  ['R'] = {
    function()
      reload_config()
      vim.cmd 'silent Sleuth'
      vim.api.nvim_echo({ { 'Config reloaded' } }, false, {})
    end,
    'Reload config',
  },
}, { prefix = '<space>' })

require('which-key').register {
  [']b'] = { '<Cmd>BufferLineCycleNext<cr>', 'Next buffer' },
  ['[b'] = { '<Cmd>BufferLineCyclePrev<cr>', 'Previous buffer' },
}
