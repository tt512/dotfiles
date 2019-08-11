"          _
"    _  __(_)_ _  ________
"  _| |/ / /  ' \/ __/ __/
" (_)___/_/_/_/_/_/  \__/
"--------------------------------------------------

" General {{{1
set foldmethod=marker
set mouse=a
set clipboard+=unnamedplus
set smartcase
set smartindent
set virtualedit=all " cursor can be positioned anywhere
set hidden          " switch buffer without saving

" Do not insert space when joining multibyte lines
set formatoptions+=mM
set formatoptions+=j " remove a comment leader

" Command-line completion
set wildmenu
set wildmode=longest:full,full
set wildignore=*.o
set wildignorecase

" Reset auto commands when reload
augroup vimrc
  autocmd!
augroup END

" Default indent level
set expandtab tabstop=4 shiftwidth=4 softtabstop=4

let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'

" Indent level for specific filetypes
au vimrc FileType html,javascript,typescript,pascal,ruby,yaml,zsh,vim,cucumber setl et ts=2 sw=2 sts=2
au vimrc FileType conf,xf86conf setl noet ts=8 sw=8 sts=8
au vimrc FileType go setl noet ts=4 sw=4 sts=4

" Install plug.vim if it is not installed yet
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
" }}}
" Bundler {{{1
call plug#begin('~/.local/share/nvim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'w0rp/ale'
Plug 'SirVer/ultisnips'
Plug 'Chiel92/vim-autoformat'
Plug 'majutsushi/tagbar'
Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-gitgutter'
Plug 'tyru/eskk.vim'
Plug 'jxnblk/vim-mdx-js'
" Completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugin' }
Plug 'zchee/deoplete-jedi'
" Python
Plug 'Glench/Vim-Jinja2-Syntax'
" HTML, JS, CSS
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'HerringtonDarkholme/yats.vim'
Plug 'othree/yajs', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'ap/vim-css-color', { 'for': ['css', 'scss', 'less', 'html'] }
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mhartington/nvim-typescript', {'do': './install.sh'}
" Other languages
Plug 'vim-jp/vim-go-extra'
Plug 'tpope/vim-cucumber'

" -- Colorscheme --
Plug 'morhetz/gruvbox'
call plug#end()
" }}}
" Appearance {{{1
set relativenumber
set list lcs=tab:→\ ,trail:·,extends:…,precedes:…
set display=uhex,lastline
set laststatus=2  " Always appear statusline
set cursorline    " Highlight cursorline

set colorcolumn=80

set termguicolors
set background=dark
colorscheme gruvbox
" }}}
" Plugins {{{1
" lightline {{{2
let g:lightline = {
      \   'active': {
      \     'right': [['lineinfo'],
      \               ['percent'],
      \               ['skk', 'fileformat', 'fileencoding', 'bomb', 'filetype']]
      \   },
      \   'component': {
      \     'bomb': '%{&bomb?"BOM":""}',
      \   },
      \   'component_visible_condition': {
      \     'bomb': '&bomb'
      \   },
      \   'subseparator': { 'left': '│', 'right': '│' },
      \ }
" }}}
" emmet-vim {{{2
let g:user_emmet_settings = {'lang': 'ja'}
" }}}
" EasyAlign {{{2
" For visual mode (e.g. vip<Enter>)
vmap <Enter>   <Plug>(EasyAlign)

" For normal mode, with Vim movement (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

let g:easy_align_delimiters = {
      \   '>': { 'pattern': '>>\|=>\|>' },
      \   '/': { 'pattern': '//\+\|/\*\|\*/', 'ignore_groups': ['String'] },
      \   '#': { 'pattern': '#\+', 'ignore_groups': ['String'], 'delimiter_align': 'l' },
      \   ']': {
      \     'pattern':       '[[\]]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \   ')': {
      \     'pattern':       '[()]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \   'd': {
      \     'pattern': ' \(\S\+\s*[;=]\)\@=',
      \     'left_margin': 0,
      \     'right_margin': 0
      \   }
      \ }
" }}}
" CtrlP {{{2
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_extensions = ['buffertag']
" }}}
" ALE {{{2
let g:ale_fixers = {
      \   'python': ['autopep8'],
      \   'javascript': ['eslint'],
      \   'typescript': ['eslint'],
      \   'c': ['clang-format'],
      \   'go': ['gofmt'],
      \ }
let g:ale_fix_on_save = 1
" }}}
" deoplete {{{2
let g:deoplete#enable_at_startup = 1
" }}}
" eskk {{{2
let g:eskk#large_dictionary = '~/SKK-JISYO.XL'
" }}}
" }}}
" keymapping {{{1
nnoremap <Space>t :NERDTreeToggle<CR>
nnoremap <Space>f :TagbarToggle<CR>
" }}}
