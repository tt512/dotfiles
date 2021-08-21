-- vim: set et ts=2 sw=2 sts=2 fdm=marker:
-- Bootstrap {{{1
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd [[packadd packer.nvim]]
end
-- }}}
-- Packages {{{1
require'packer'.startup({
  function(use, use_rocks)
    use 'wbthomason/packer.nvim'

    use 'neovim/nvim-lspconfig'
    use 'kabouzeid/nvim-lspinstall'
    use {'glepnir/lspsaga.nvim', config = function() require'lspsaga'.init_lsp_saga() end}

    use 'hrsh7th/nvim-compe'

    use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}}
    use {'lambdalisue/fern.vim', 'lambdalisue/fern-git-status.vim'}
    use 'liuchengxu/vista.vim'
    use 'hoob3rt/lualine.nvim'
    use {'akinsho/nvim-toggleterm.lua', config = function() require'toggleterm'.setup() end}

    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

    use {'lukas-reineke/indent-blankline.nvim'}
    use 'lukas-reineke/format.nvim'
    use 'kyazdani42/nvim-web-devicons'
    use 'ojroques/vim-oscyank'
    use 'windwp/nvim-autopairs'
    use 'ggandor/lightspeed.nvim'
    use 'tyru/eskk.vim'
    use 'ludovicchabant/vim-gutentags'

    use {"rcarriga/vim-ultest", requires = {"vim-test/vim-test"}, run = ":UpdateRemotePlugins"}
    use {"rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"}}
    use "Pocco81/DAPInstall.nvim"

    use 'lambdalisue/gina.vim'
    use {'ruifm/gitlinker.nvim', requires = 'nvim-lua/plenary.nvim'}
    use {
      'lewis6991/gitsigns.nvim',
      requires = 'nvim-lua/plenary.nvim',
      config = function() require'gitsigns'.setup() end
    }

    use 'folke/tokyonight.nvim'
  end,
  config = {display = {open_fn = require'packer.util'.float}}
})
-- }}}1
-- General Settings {{{1
vim.opt.virtualedit = 'all'
vim.opt.mouse = 'a'
vim.opt.cursorline = true
vim.opt.undofile = true
vim.opt.smartindent = true

vim.opt.wildmode = {'longest:full', 'full'}
vim.opt.wildignore = '*.o'
vim.opt.wildignorecase = true
vim.opt.hidden = true

vim.opt.termguicolors = true

vim.opt.clipboard:append('unnamedplus')
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

vim.cmd [[colorscheme tokyonight]]
-- }}}1
-- LSP {{{1
local function setup_servers()
  require'lspinstall'.setup()
  local servers = require'lspinstall'.installed_servers()
  for _, server in pairs(servers) do
    local on_attach = function(client, bufnr)
      vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', {noremap = true, silent = true})
    end

    local config = {on_attach = on_attach}
    if server == "lua" then config.settings = {Lua = {diagnostics = {globals = {'vim'}}}} end
    require'lspconfig'[server].setup(config)
  end
end
setup_servers()
-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end
-- }}}1
-- Completion {{{1
vim.opt.completeopt = {'menuone', 'noselect'}

require'compe'.setup {source = {path = true, buffer = true, calc = true, nvim_lsp = true, nvim_lua = true}}

local t = function(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end

local check_back_space = function()
  local col = vim.fn.col('.') - 1
  return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn['vsnip#available'](1) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn['vsnip#jumpable'](-1) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<CR>", [[compe#confirm(luaeval("require 'nvim-autopairs'.autopairs_cr()"))]],
                        {expr = true})
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

require'nvim-autopairs'.setup {}
require("nvim-autopairs.completion.compe").setup({
  map_cr = true, --  map <CR> on insert mode
  map_complete = true, -- it will auto insert `(` after select function or method item
  auto_select = false -- auto select first item
})
-- }}}1
-- Debugging {{{1
vim.api.nvim_set_keymap('n', '<F5>', [[<cmd>lua require'dap'.continue()<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<F10>', [[<cmd>lua require'dap'.step_over()<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<F11>', [[<cmd>lua require'dap'.step_into()<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<F12>', [[<cmd>lua require'dap'.step_out()<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>b', [[<cmd>lua require'dap'.toggle_breakpoint()<CR>]],
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>B',
                        [[<cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>]],
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>lp',
                        [[<cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>]],
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>dr', [[<cmd>lua require'dap'.repl.open()<CR>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>dl', [[<cmd>lua require'dap'.run_last()<CR>]], {noremap = true, silent = true})

local dap_install = require("dap-install")
local dbg_list = require("dap-install.debuggers_list").debuggers

for debugger, _ in pairs(dbg_list) do dap_install.config(debugger, {}) end

require'dapui'.setup()

require'ultest'.setup {
  builders = {
    ["go#gotest"] = function(cmd)
      local args = {}
      for i = 3, #cmd - 1, 1 do
        local arg = cmd[i]
        if vim.startswith(arg, "-") then
          -- Delve requires test flags be prefix with 'test.'
          arg = "-test." .. string.sub(arg, 2)
        end
        args[#args + 1] = arg
      end
      return {
        dap = {
          type = "go",
          request = "launch",
          mode = "test",
          program = "${workspaceFolder}",
          dlvToolPath = vim.fn.exepath("dlv"),
          args = args
        },
        parse_result = function(lines) return lines[#lines] == "FAIL" and 1 or 0 end
      }
    end
  }
}
-- }}}1
-- Format {{{1
require'format'.setup {lua = {{cmd = {'lua-format -i'}}}, rust = {{cmd = {'rustfmt'}}}, go = {{cmd = {'gofmt -w'}}}}
vim.cmd [[
augroup Format
  autocmd!
  autocmd BufWritePost * FormatWrite
augroup END
]]
-- }}}1
-- OSCYank {{{1
vim.cmd [[
augroup OSCYank
  autocmd!
  autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | OSCYankReg " | endif
augroup END
]]
-- }}}1
-- Gitlinker {{{1
require'gitlinker'.setup {
  opts = {
    action_callback = function(url)
      vim.fn.setreg('+', url)
      local backup = vim.g.oscyank_silent
      vim.g.oscyank_silent = true
      vim.cmd [[OSCYankReg +]]
      vim.g.oscyank_silent = backup
    end
  }
}
-- }}}1
-- Lualine {{{1
local nerdtree = require('lualine.extensions.nerdtree')
local lualine_fern = {sections = vim.deepcopy(nerdtree.sections), filetypes = {'fern'}}
local lualine_vista = {sections = {lualine_a = {function() return vim.g.vista.provider end}}, filetypes = {'vista'}}
require'lualine'.setup {
  options = {theme = 'tokyonight', section_separators = '', component_separators = '│', icons_enabled = false},
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {lualine_fern, lualine_vista}
}
-- }}}1
-- Fern {{{1
vim.g['fern#renderer#default#leaf_symbol'] = '   '
vim.g['fern#renderer#default#collapsed_symbol'] = ' ⯈ '
vim.g['fern#renderer#default#expanded_symbol'] = ' ⯆ '
vim.g['fern#disable_viewer_hide_cursor'] = 1
_G.init_fern = function()
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
  vim.api.nvim_buf_set_keymap(0, 'n', '<Plug>(fern-my-open-or-toggle)', vim.fn['fern#smart#leaf'](
                                  '<Plug>(fern-action-open)', '<Plug>(fern-action-expand)',
                                  '<Plug>(fern-action-collapse)'), {})
  vim.api.nvim_buf_set_keymap(0, 'n', '<2-LeftMouse>', '<Plug>(fern-my-open-or-toggle)', {})
end
vim.cmd [[
augroup my_fern
   au!
   au Filetype fern call v:lua.init_fern()
augroup END
]]
-- }}}1
-- Telescope {{{1
local actions = require('telescope.actions')
require'telescope'.setup {
  defaults = {
    layout_strategy = 'vertical',
    mappings = {
      i = {
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
        ['<esc>'] = actions.close
      }
    }
  },
  pickers = {buffers = {sort_lastused = true}}
}
-- }}}1
-- Treesitter {{{1
require'nvim-treesitter.configs'.setup {ensure_installed = 'maintained', highlight = {enable = true}}
-- }}}1
-- Indent Blankline {{{1
require'indent_blankline'.setup {filetype_exclude = {'help', 'fern', 'vista'}, use_treesitter = true}
-- }}}1
-- Global Keymappings {{{1
vim.api.nvim_set_keymap('n', 'gr', ':<C-u>Telescope lsp_references<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'gd', ':<C-u>Telescope lsp_definitions<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'gi', ':<C-u>Telescope lsp_implementations<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'ga', ':<C-u>Lspsaga code_action<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'gn', ':<C-u>Lspsaga rename<cr>', {noremap = true, silent = true})

vim.api.nvim_set_keymap('n', '<space>b', ':<C-u>Telescope buffers<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>f', ':<C-u>Telescope find_files<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>c', ':<C-u>Telescope command_history<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>r', ':<C-u>Telescope oldfiles<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>m', ':<C-u>Vista<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>o', ':<C-u>Telescope treesitter<cr>', {noremap = true, silent = true})

vim.api.nvim_set_keymap('n', '<space>t', ':<C-u>Fern . -drawer<cr>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>R', ':<C-u>so $MYVIMRC<cr>:echo "reloaded vimrc."<cr>',
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<space>E', ':<C-u>e $MYVIMRC<cr>', {noremap = true, silent = true})
-- }}}1
