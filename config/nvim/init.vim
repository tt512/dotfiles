"          _
"    _  __(_)_ _  ________
"  _| |/ / /  ' \/ __/ __/
" (_)___/_/_/_/_/_/  \__/
"--------------------------------------------------

" Initial Settings {{{1
scriptencoding utf8

" Reset autocommands when reload
augroup vimrc
  autocmd!
augroup END

" Install plug.vim if it is not installed yet
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd vimrc VimEnter * PlugInstall
endif
" }}}
" Plugin Management {{{1
call plug#begin('~/.local/share/nvim/plugged')
Plug 'junegunn/vim-plug'
Plug 'vim-airline/vim-airline'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'majutsushi/tagbar'
Plug 'liuchengxu/vim-clap'
Plug 'markwu/vim-mrufiles'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'mhinz/vim-startify'
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'lambdalisue/gina.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'tyru/eskk.vim'
" Language Specific Plugins
Plug 'pangloss/vim-javascript'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mattn/emmet-vim'
Plug 'ap/vim-css-color', { 'for': ['css', 'scss', 'less', 'html'] }
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'jxnblk/vim-mdx-js'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'tpope/vim-cucumber'
" Color Schemes
Plug 'gruvbox-community/gruvbox'
call plug#end()
" }}}
" General Settings {{{1
set foldmethod=marker
set mouse=a
set clipboard+=unnamedplus
set smartcase
set smartindent
set virtualedit=all " cursor can be positioned anywhere
set hidden          " switch buffer without saving
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes
set undofile

" Do not insert space when joining multibyte lines
set formatoptions+=mM
set formatoptions+=j " remove a comment leader

" Command-line completion
set wildmenu
set wildmode=longest:full,full
set wildignore=*.o
set wildignorecase

set relativenumber
set list lcs=tab:→\ ,trail:·,extends:…,precedes:…
set display=uhex,lastline
set laststatus=2  " Always appear statusline
set cursorline    " Highlight cursorline

set colorcolumn=80

set termguicolors
set background=dark
colorscheme gruvbox

" restore cursor
au vimrc BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

" Default indent level
set expandtab tabstop=4 shiftwidth=4 softtabstop=4

" Indent level for specific filetypes
au vimrc FileType html,javascript,typescript,javascriptreact,typescriptreact,pascal,ruby,yaml,zsh,cucumber setl et ts=2 sw=2 sts=2
au vimrc FileType conf,xf86conf setl noet ts=8 sw=8 sts=8
au vimrc FileType go setl noet ts=4 sw=4 sts=4

let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'
" }}}
" Plugins Settings {{{1
" vim-plug {{{2
let g:plug_window = 'tab new'
command! PU PlugUpdate | PlugUpgrade
" }}}
" vim-airline {{{2
let g:airline_symbols_ascii = 1
let g:airline_mode_map = {
      \ '__'     : '-',
      \ 'c'      : 'C',
      \ 'i'      : 'I',
      \ 'ic'     : 'I',
      \ 'ix'     : 'I',
      \ 'n'      : 'N',
      \ 'multi'  : 'M',
      \ 'ni'     : 'N',
      \ 'no'     : 'N',
      \ 'R'      : 'R',
      \ 'Rv'     : 'R',
      \ 's'      : 'S',
      \ 'S'      : 'S',
      \ ''     : 'S',
      \ 't'      : 'T',
      \ 'v'      : 'V',
      \ 'V'      : 'V',
      \ ''     : 'V',
      \ }
" }}}
" emmet-vim {{{2
let g:user_emmet_settings = {'lang': 'ja'}
" }}}
" EasyAlign {{{2
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
" eskk {{{2
let g:eskk#large_dictionary = '~/SKK-JISYO.XL'
" }}}
" Startify {{{2
let g:startify_change_to_dir = 0
" }}}
" Fern {{{
let g:fern#renderer#default#leaf_symbol = '   '
let g:fern#renderer#default#collapsed_symbol = ' ⯈ '
let g:fern#renderer#default#expanded_symbol = ' ⯆ '
let g:fern#disable_viewer_hide_cursor = 1
function! s:init_fern() abort
    setl nonu nornu
    nmap <buffer><expr>
          \ <Plug>(fern-my-open-or-expand-or-collapse)
          \ fern#smart#leaf(
          \   "\<Plug>(fern-action-open)",
          \   "\<Plug>(fern-action-expand)",
          \   "\<Plug>(fern-action-collapse)",
          \ )
    nmap <buffer> <2-LeftMouse> <Plug>(fern-my-open-or-expand-or-collapse)
endfunction
augroup my_fern
    au!
    au Filetype fern call s:init_fern()
augroup END
" }}}
" vim-lsp {{{
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> <leader>rn <plug>(lsp-rename)
    nmap <buffer> [g <Plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <Plug>(lsp-next-diagnostic)
    nmap <buffer> K <plug>(lsp-hover)
endfunction
augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
let g:lsp_highlight_enabled = 1
let g:lsp_virtual_text_enabled = 1
let g:lsp_diagnostigs_float_cursor = 1
let g:lsp_documentation_float = 1
let g:lsp_diagnostics_echo_cursor = 1
hi! LspErrorHighlight cterm=undercurl gui=undercurl guisp=#fb4934
hi! LspWarningHighlight cterm=undercurl gui=undercurl guisp=#fe8019
hi! LspInfomationHighlight cterm=undercurl gui=undercurl guisp=#fabd2f
hi! LspHintHighlight cterm=undercurl gui=undercurl guisp=#83a598

hi! link LspErrorText GruvboxRedSign
hi! link LspWarningText GruvboxYellowSign
hi! link LspInformationText GruvboxBlueSign
hi! link LspHintText GruvboxBlueSign
" }}}
" vim-clap {{{
let g:clap_insert_mode_only = 1
" }}}
" }}}
" Key Mappings {{{1
" EasyAlign
" For visual mode (e.g. vip<Enter>)
vmap <Enter>   <Plug>(EasyAlign)
" For normal mode, with Vim movement (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

" vim-clap
nnoremap <silent> <space>b  :<C-u>Clap buffers<cr>
nnoremap <silent> <space>f  :<C-u>Clap files<cr>
nnoremap <silent> <space>r  :<C-u>Clap mrufiles<cr>
nnoremap <silent> <space>g  :<C-u>Clap grep<cr>

nnoremap <silent> <space>t  :<C-u>Fern . -drawer<CR>
nnoremap <silent> <space>m  :<C-u>TagbarOpen<CR>

nnoremap <silent> <space>R :<C-u>so $MYVIMRC<cr>:echo 'vimrc reloaded'<cr>
nnoremap <silent> <space>E :<C-u>e $MYVIMRC<cr>
" }}}
