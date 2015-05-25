"          _
"    _  __(_)_ _  ________
"  _| |/ / /  ' \/ __/ __/
" (_)___/_/_/_/_/_/  \__/    author: tatarhy
"--------------------------------------------------

" General {{{1
"set nocompatible
""カーソルを行頭、行末で止まらないようにする
"set whichwrap=b,s,h,l,<,>,[,]
"set backspace=2 "改行を削除できるように
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
au vimrc FileType coffee,html,javascript,ruby,yaml setl et ts=2 sw=2 sts=2
au vimrc FileType conf,xf86conf setl noet ts=8 sw=8 sts=8

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
Plug 'kien/ctrlp.vim'
Plug 'itchyny/lightline.vim'
Plug 'Shougo/neocomplete'
Plug 'thinca/vim-template'
Plug 'soramugi/auto-ctags.vim'
"Plug 'thinca/vim-quickrun', {'autoload': {'on': 'Quickrun'}}
"Plug 'beloglazov/vim-online-thesaurus'
"Plug 'fweep/vim-zsh-path-completion'
Plug 'scrooloose/syntastic'
"Plug 'LaTeX-Box-Team/LaTeX-Box', {'for' : 'tex'}
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-surround'
"Plug 'rhysd/vim-clang-format'
Plug 'mattn/emmet-vim', { 'for': ['html', 'css', 'scss', 'eruby'] }
"Plug 'sjl/gundo.vim', { 'on': ['GundoShow', 'GundoToggle'] }
"Plug 'vim-jp/vimdoc-ja'
Plug 'gregsexton/MatchTag', { 'for': 'html' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'evidens/vim-twig', { 'for': 'twig' }
" -- Colorscheme --
"Plug 'tomasr/molokai'
"Plug 'w0ng/vim-hybrid'
"Plug 'altercation/vim-colors-solarized'
Plug 'morhetz/gruvbox'
Plug 'jpo/vim-railscasts-theme'
call plug#end()
""" }}}
" Appearance {{{1
set relativenumber
syntax on
set list " Tab、行末の半角スペースを明示的に表示する。
set listchars=tab:>\ ,trail:~,extends:>,precedes:< " 不可視文字の表示形式
set display=uhex  " 印字不可能文字を16進数で表示

set laststatus=2  " Always appear statusline
set cursorline    " Highlight cursorline

" □や○の文字があってもカーソル位置がずれないようにする。
"set ambiwidth=double
" 画面最後の行をできる限り表示する。
set display+=lastline

" 80桁目をハイライト
set colorcolumn=80
"setlocal textwidth=0
"if exists('&colorcolumn')
"  setlocal colorcolumn=+1
"  autocmd FileType * setlocal textwidth=80
"endif

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
      \              [ 'fileformat', 'fileencoding', 'bomb', 'filetype' ] ]
      \ },
      \ 'component': {
      \   'bomb': '%{&bomb?"BOM":""}'
      \ },
      \ 'component_visible_condition': {
      \   'bomb': '&bomb'
      \ }
      \ }
" }}}
" emmet-vim {{{2
let g:user_emmet_settings = {'lang': 'ja'}
" }}}
" LaTeX-Box {{{2
"let g:tex_flavor = 'latex'
"let g:LatexBox_output_type = 'pdf'
"let g:LatexBox_viewer = 'xdg-open'
"let g:LatexBox_latexmk_options = "-pdflatex='xelatex -synctex=1 \%O \%S'"
"let g:LatexBox_ignore_warnings
"      \ = ['Underfull', 'Overfull', 'specifier changed to', 'redefine-command']
" }}}
" syntastic {{{2
let g:syntastic_mode_map = {
    \ 'mode': 'active',
    \ 'active_filetypes': ['c', 'c++', 'sass', 'ruby', 'vala', 'javascript'],
    \ 'passive_filetypes': []
    \ }
"let g:syntastic_quiet_warnings = 0
"let g:syntastic_splint_config_file
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_c_checkers=['gcc', 'cppcheck', 'splint']
"let g:syntastic_cpp_compiler = 'clang++'
"let g:syntastic_cpp_checkers=["g++", 'cppcheck']
let g:syntastic_vala_checkers = ['valac']
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
" neocomplete {{{2
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_ignore_case = 1
let g:neocomplete#enable_smart_case = 1
if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns._ = '\h\w*'
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

"let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
"
"" Define dictionary.
"let g:neocomplete#sources#dictionary#dictionaries = {
"      \ 'default' : '',
"      \ 'vimshell' : $HOME.'/.vimshell_hist',
"      \ 'scheme' : $HOME.'/.gosh_completions'
"      \ }
"
"" Define keyword.
"if !exists('g:neocomplete#keyword_patterns')
"  let g:neocomplete#keyword_patterns = {}
"endif
"let g:neocomplete#keyword_patterns['default'] = '\h\w*'
"
"" Plugin key-mappings.
"inoremap <expr><C-g>     neocomplete#undo_completion()
"inoremap <expr><C-l>     neocomplete#complete_common_string()
"
"" Recommended key-mappings.
"" <CR>: close popup and save indent.
"inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
"function! s:my_cr_function()
"  return neocomplete#smart_close_popup() . "\<CR>"
"  " For no inserting <CR> key.
"  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
"endfunction
"" <TAB>: completion.
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"" <C-h>, <BS>: close popup and delete backword char.
"inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
"inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
"inoremap <expr><C-y>  neocomplete#close_popup()
"inoremap <expr><C-e>  neocomplete#cancel_popup()
"" Close popup by <Space>.
""inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"
"
"" Enable omni completion.
"autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"" Enable heavy omni completion.
"if !exists('g:neocomplete#sources#omni#input_patterns')
"  let g:neocomplete#sources#omni#input_patterns = {}
"endif
""let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
""let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
""let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
"
"" }}}
" CtrlP {{{2
let g:ctrlp_cmd = 'CtrlPBuffer'
"if executable('ag')
"  let g:ctrlp_use_caching = 0
"  let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
"endif
" }}}
" vim-template {{{2
autocmd User plugin-template-loaded
            \ if search('<+CURSOR+>')
            \ |  execute 'normal! "_da>'
            \ | endif
" }}}
" auto-ctags {{{2
"let g:auto_ctags_directory_list = ['.git']
" }}}
"" }}}
" keymapping {{{1
" カーソルを表示行で移動
"nnoremap j gj
"nnoremap k gk
"" }}}

