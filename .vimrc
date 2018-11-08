"          _
"    _  __(_)_ _  ________
"  _| |/ / /  ' \/ __/ __/
" (_)___/_/_/_/_/_/  \__/
"--------------------------------------------------

" General {{{1
set encoding=utf-8
set fileencodings=utf-8,cp932,iso-2022-jp,euc-jp,default,latin
set foldmethod=marker
set clipboard=unnamedplus,unnamed,autoselect

" Command-line completion
set wildmenu
set wildmode=longest:full,full
set wildignore=*.o
set wildignorecase

set ignorecase
set smartcase

" Reset auto commands when reload
augroup vimrc
  autocmd!
augroup END

" Default indent level
set expandtab tabstop=4 shiftwidth=4 softtabstop=4

" Indent level for specific filetypes
au vimrc FileType html,javascript,pascal,ruby,yaml,zsh setl et ts=2 sw=2 sts=2
au vimrc FileType conf,xf86conf setl noet ts=8 sw=8 sts=8

au FileType go setl noet ts=4 sw=4 sts=4 makeprg=go\ build\ ./... errorformat=%f:%l:\ %m
au FileType go autocmd BufWritePre <buffer> Fmt

" Open Quickfix automatically after execute some commands
au QuickfixCmdPost make,grep,grepadd,vimgrep cwindow

" Do not insert space when joining multibyte lines
set formatoptions+=mM
set formatoptions+=j " remove a comment leader
set autoindent
set smartindent

set virtualedit=all " cursor can be positioned anywhere
set hidden          " switch buffer without saving

"set magic " Special characters have special meaning without backslash
"set autochdir " Change directory automatically

set directory=~/.vim/swap " Directory for the swap file

if has('persistent_undo')
  set undodir=~/.vim/undo
  set undofile
endif

" Restore cursor position
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

if !exists('loaded_matchit')
  runtime macros/matchit.vim
endif

" Install plug.vim if it is not installed yet
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

" }}}
" Bundler {{{1
call plug#begin('~/.vim/bundle')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'mattn/sonictemplate-vim'
Plug 'w0rp/ale'
Plug 'maralla/completor.vim'
Plug 'SirVer/ultisnips'
Plug 'tyru/skk.vim'
Plug 'Chiel92/vim-autoformat'
Plug 'majutsushi/tagbar'
Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-gitgutter'
" Python
Plug 'Glench/Vim-Jinja2-Syntax'
" HTML, JS, CSS
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'othree/yajs', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'ap/vim-css-color', { 'for': ['css', 'scss', 'less', 'html'] }
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
" Other languages
Plug 'vim-jp/vim-go-extra'

"Plug 'tyru/eskk.vim'
"Plug 'gregsexton/MatchTag', { 'for': 'html' }

" -- Colorscheme --
Plug 'morhetz/gruvbox'
call plug#end()
""" }}}
" Appearance {{{1
set relativenumber
set list lcs=tab:>\ ,trail:~,extends:>,precedes:<
set display=uhex,lastline
set laststatus=2  " Always appear statusline
set cursorline    " Highlight cursorline

syntax on
filetype plugin indent on

set colorcolumn=80

set t_Co=256
set background=dark
colorscheme gruvbox
" }}}
" Plugins {{{1
" lightline {{{2
let g:lightline = {
      \ 'active': {
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'skk', 'fileformat', 'fileencoding', 'bomb', 'filetype' ] ]
      \ },
      \ 'component': {
      \   'bomb': '%{&bomb?"BOM":""}',
      \ },
      \ 'component_visible_condition': {
      \   'bomb': '&bomb'
      \ },
      \ 'component_function': {
      \   'skk': 'LightLineSkk',
      \ },
      \ }

"      \ 'component_function': {
"      \   'eskk': 'LightLineEskk',
"      \ }

function! LightLineSkk()
    return SkkGetModeStr()
endfunction

"function! LightLineEskk()
"    return eskk#is_enabled()
"                \ ? get(g:eskk#statusline_mode_strings,
"                \       eskk#get_current_instance().mode, '??')
"                \ : ''
"endfunction
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
      \ '>': { 'pattern': '>>\|=>\|>' },
      \ '/': { 'pattern': '//\+\|/\*\|\*/', 'ignore_groups': ['String'] },
      \ '#': { 'pattern': '#\+', 'ignore_groups': ['String'], 'delimiter_align': 'l' },
      \ ']': {
      \     'pattern':       '[[\]]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ ')': {
      \     'pattern':       '[()]',
      \     'left_margin':   0,
      \     'right_margin':  0,
      \     'stick_to_left': 0
      \   },
      \ 'd': {
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
" vim-template {{{2
autocmd User plugin-template-loaded
            \ if search('<+CURSOR+>')
            \ |  execute 'normal! "_da>'
            \ | endif
" }}}
" sonictemplate {{{
let g:sonictemplate_vim_template_dir = '$HOME/.vim/template'
" }}}
" ALE {{{
let g:ale_fixers = {
            \   'python': [
            \     'autopep8',
            \   ],
            \   'javascript': [
            \     'eslint',
            \   ],
            \ }
let g:ale_fix_on_save = 1
" }}}
" eskk {{{
"if has('vim_starting')
"    let g:eskk#dictionary = {
"                \     'path': '~/.skk-jisyo',
"                \     'sorted': 0,
"                \     'encoding': 'euc-jp',
"                \ }
"    let g:eskk#large_dictionary = {
"                \     'path': '~/SKK-JISYO.XL',
"                \     'sorted': 1,
"                \     'encoding': 'euc-jp',
"                \ }
"endif
"let g:eskk#enable_completion = 1
" }}}
" skk {{{
let g:skk_large_jisyo = '~/SKK-JISYO.XL'
let g:skk_auto_save_jisyo = 1
" }}}
" completor {{{
" Use Tab to select completion
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

let g:completor_clang_binary = '/usr/bin/clang'
" }}}
"" }}}
" keymapping {{{1
nnoremap <Space>t :NERDTreeToggle<CR>
nnoremap <Space>f :TagbarToggle<CR>
"" }}}

