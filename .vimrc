""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"         _
"   _  __(_)_ _  ________
" _| |/ / /  ' \/ __/ __/
"(_)___/_/_/_/_/_/  \__/
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" General {{{
"set nocompatible
""カーソルを行頭、行末で止まらないようにする
"set whichwrap=b,s,h,l,<,>,[,]
"set backspace=2 "改行を削除できるように
set encoding=utf-8 "デフォルトエンコーディング
" 文字コードの自動認識
set fileencodings=utf-8,cp932,iso-2022-jp,euc-jp,default,latin
set foldmethod=marker
set clipboard=unnamedplus,unnamed,autoselect

" コマンドライン補完
set wildmenu
set wildmode=longest:full,full
set wildignore=*.o
set wildignorecase
set ignorecase
set smartcase

set expandtab
set tabstop=2
set shiftwidth=0 " use tabstop value
" Do not insert space when joining multibyte lines
set formatoptions+=mM
set formatoptions+=j " remove a comment leader
set autoindent
set smartindent

set virtualedit=all " 文字がないところにも移動できる
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

" }}}
" Appearance {{{
"set number "行番号表示
set nowrap " 折り返さない
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
setlocal textwidth=0
if exists('&colorcolumn')
  setlocal colorcolumn=+1
  autocmd FileType sh,c,cpp,perl,vim,ruby,python,haskell,scheme setlocal textwidth=80
endif
" }}}
" Plugins {{{
" NeoBundle {{{
" neobundle settings {{{
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
" Initialize neobundle.vim
call neobundle#rc(expand('~/.vim/bundle/'))
" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'
" Update and build automatically
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }
" }}}
"NeoBundleLazy 'thinca/vim-quickrun', {'autoload': {'commands': 'Quickrun'}}
"NeoBundleLazy 'jcf/vim-latex', { 'autoload': { 'filetypes' : ['tex'] }}
"NeoBundle 'beloglazov/vim-online-thesaurus'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'fweep/vim-zsh-path-completion'
NeoBundle 'thinca/vim-template'
NeoBundle 'scrooloose/syntastic'
NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {'autoload' : {'filetypes' : ['tex']}}
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'tpope/vim-surround'
NeoBundle 'rhysd/vim-clang-format'
NeoBundleLazy 'mattn/emmet-vim', {'autoload': {'filetypes': ['html', 'css', 'scss', 'eruby']}}
NeoBundleLazy 'sjl/gundo.vim', {'autoload': {'commands': ['GundoShow', 'GundoToggle']}}
NeoBundle 'rcmdnk/vim-markdown'
NeoBundle 'vim-jp/vimdoc-ja'
NeoBundleLazy 'tkztmk/vim-vala', {'autoload': {'filetypes': 'vala'}}
"NeoBundle 'kchmck/vim-coffee-script'
"NeoBundleLazy 'skammer/vim-css-color', {'autoload': {'filetypes': 'css'}}
"NeoBundleLazy 'lilydjwg/colorizer', {'autoload': {'filetypes': 'css'}}
"NeoBundleLazy 'pangloss/vim-javascript', {'autoload': {'filetypes': 'javascript'}}
NeoBundleLazy 'Shougo/neocomplete', {
      \ 'depends':     ['Shougo/neosnippet', 'Shougo/context_filetype.vim'],
      \ 'disabled':    !has('lua'),
      \ 'vim_version': '7.3.885',
      \ 'autoload':    {'insert' : 1,}
      \ }
" Colorscheme files {{{
"NeoBundle 'tomasr/molokai'
"NeoBundle 'jpo/vim-railscasts-theme'
NeoBundle 'w0ng/vim-hybrid'
"NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'hukl/Smyck-Color-Scheme', {'script_type' : 'colors'}
"}}}

filetype plugin indent on     " Required!
NeoBundleCheck " Installation check.

""" }}}
" colorscheme {{{
" solarized
"syntax enable
"set background=dark
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
"colorscheme solarized

set t_Co=256
"set background=dark
""let g:hybrid_use_Xresources = 1
"colorscheme hybrid

"let g:molokai_original = 1
"let g:rehash256 = 1
"colorscheme molokai

"Smyck
colorscheme smyck
highlight Folded     ctermbg=0
highlight CursorLine ctermbg=0
highlight Visual     ctermbg=8 ctermfg=none

highlight Normal ctermbg=none
" }}}
" lightline {{{
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
" emmet-vim {{{
let g:user_emmet_settings = {'lang': 'ja'}
" }}}
" LaTeX-Box {{{
let g:LatexBox_latexmk_options = "-pdflatex='xelatex -synctex=1 \%O \%S'"
let g:LatexBox_ignore_warnings
      \ = ['Underfull', 'Overfull', 'specifier changed to', 'redefine-command']
" }}}
" syntastic {{{
let g:syntastic_mode_map = {
    \ 'mode': 'active',
    \ 'active_filetypes': ['c', 'c++', 'sass', 'ruby', 'vala'],
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
" EasyAlign {{{
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
" neocomplete {{{
if neobundle#is_installed('neocomplete')
  " neocomplete用設定
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_ignore_case = 1
  let g:neocomplete#enable_smart_case = 1
  if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
    endif
  let g:neocomplete#keyword_patterns._ = '\h\w*'
  " <TAB>: completion.
  inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
endif
"if has('lua')
"  let s:bundle = neobundle#get('neocomplete')
"  function! s:bundle.hooks.on_source(bundle)
"    let g:neocomplete#enable_at_startup = 1 " Use neocomplete.
"    let g:neocomplete#enable_smart_case = 1 " Use smartcase.
"    " Set minimum syntax keyword length.
"    let g:neocomplete#sources#syntax#min_keyword_length = 3
"    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
"
"    " Define dictionary.
"    let g:neocomplete#sources#dictionary#dictionaries = {
"          \ 'default' : '',
"          \ 'vimshell' : $HOME.'/.vimshell_hist',
"          \ 'scheme' : $HOME.'/.gosh_completions'
"          \ }
"
"    " Define keyword.
"    if !exists('g:neocomplete#keyword_patterns')
"      let g:neocomplete#keyword_patterns = {}
"    endif
"    let g:neocomplete#keyword_patterns['default'] = '\h\w*'
"
"    " Plugin key-mappings.
"    inoremap <expr><C-g>     neocomplete#undo_completion()
"    inoremap <expr><C-l>     neocomplete#complete_common_string()
"
"    " Recommended key-mappings.
"    " <CR>: close popup and save indent.
"    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
"    function! s:my_cr_function()
"      return neocomplete#smart_close_popup() . "\<CR>"
"      " For no inserting <CR> key.
"      "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
"    endfunction
"    " <TAB>: completion.
"    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"    " <C-h>, <BS>: close popup and delete backword char.
"    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
"    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
"    inoremap <expr><C-y>  neocomplete#close_popup()
"    inoremap <expr><C-e>  neocomplete#cancel_popup()
"    " Close popup by <Space>.
"    "inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"
"
"    " For cursor moving in insert mode(Not recommended)
"    "inoremap <expr><Left>  neocomplete#close_popup() . "\<Left>"
"    "inoremap <expr><Right> neocomplete#close_popup() . "\<Right>"
"    "inoremap <expr><Up>    neocomplete#close_popup() . "\<Up>"
"    "inoremap <expr><Down>  neocomplete#close_popup() . "\<Down>"
"    " Or set this.
"    "let g:neocomplete#enable_cursor_hold_i = 1
"    " Or set this.
"    "let g:neocomplete#enable_insert_char_pre = 1
"
"    " Shell like behavior(not recommended).
"    "set completeopt+=longest
"    "let g:neocomplete#enable_auto_select = 1
"    "let g:neocomplete#disable_auto_complete = 1
"    "inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"
"
"    " Enable omni completion.
"    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"    " Enable heavy omni completion.
"    if !exists('g:neocomplete#sources#omni#input_patterns')
"      let g:neocomplete#sources#omni#input_patterns = {}
"    endif
"    "let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"    "let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"    "let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
"
"    " For perlomni.vim setting.
"    " https://github.com/c9s/perlomni.vim
"    let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
"  endfunction
"" }}}
"" vim-latex {{{
""let s:bundle = neobundle#get('vim-latex')
""function! s:bundle.hooks.on_source(bundle)
"  filetype plugin on
"  set shellslash
"  set grepprg=grep\ -nH\ $*
"  filetype indent on
"  let g:tex_flavor='latex'
"  let g:Imap_UsePlaceHolders = 1
"  let g:Imap_DeleteEmptyPlaceHolders = 1
"  let g:Imap_StickyPlaceHolders = 0
"  let g:Tex_DefaultTargetFormat = 'pdf'
"  " dependency chain: .tex -> .pdf
"  let g:Tex_FormatDependency_pdf = 'pdf'
"  let g:Tex_FormatDependency_ps = 'ps'
"  let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 -interaction=nonstopmode -file-line-error-style $*'
"  "let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 -interaction=nonstopmode $*'
"  let g:Tex_BibtexFlavor = 'pbibtex'
"  "let g:Tex_BibtexFlavor = 'upbibtex'
"  "let g:Tex_BibtexFlavor = 'bibtex'
"  "let g:Tex_BibtexFlavor = 'bibtexu'
"  let g:Tex_MakeIndexFlavor = 'mendex $*.idx'
"  "let g:Tex_MakeIndexFlavor = 'makeindex $*.idx'
"  "let g:Tex_MakeIndexFlavor = 'texindy $*.idx'
"  let g:Tex_UseEditorSettingInDVIViewer = 1
"  let g:Tex_ViewRule_pdf = 'evince'
"  let g:Tex_ViewRule_ps = 'evince'
"  let g:Tex_ViewRule_dvi = 'xdvi -watchfile 1'
"  let g:Tex_IgnoredWarnings =
"        \"Underfull\n".
"        \"Overfull\n".
"        \"specifier changed to\n".
"        \"You have requested\n".
"        \"Missing number, treated as zero.\n".
"        \"There were undefined references\n".
"        \"Citation %.%# undefined\n".
"        \"Command %.%# invalid in math mode\n".
"        \"Text page %.%# contains only floats\n"
"        \"A float is stuck\n"
"        \"Float too large\n"
"        \"LaTeX Font Warning:"
"        \"Label `' multiply defined."
"        \"There were multiply-defined labels."
"
"  let g:Tex_IgnoreLevel = 12
"  let g:Tex_GotoError = 0
""endfunction
""}}}
"" }}}
" keymapping {{{
"nnoremap <silent> [unite]v :VimFilerExplorer<CR>

" カーソルを表示行で移動
nnoremap j gj
nnoremap k gk

" スペースでページ送り
"nnoremap <Space> <PageDown>

" emacs like keybinds in insert mode
"inoremap <C-f> <Right>
"inoremap <C-b> <Left>
"inoremap <C-p> <Up>
"inoremap <C-n> <Down>
"inoremap <C-a> <Home>
"inoremap <C-e> <End>

"" }}}

