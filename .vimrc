"         _
"   _  __(_)_ _  ________
" _| |/ / /  ' \/ __/ __/
"(_)___/_/_/_/_/_/  \__/

source ~/.simplenoterc
" General {{{
set nocompatible "Vi互換をオフ
set expandtab
set tabstop=2 "タブの文字数
set shiftwidth=2
"新しい行を作った時に高度な自動インデントを行う
set autoindent
set smartindent
"カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]
set backspace=2 "改行を削除できるように
set encoding=utf-8 "デフォルトエンコーディング
"文字コードの自動認識
set fileencodings=utf-8,cp932,iso-2022-jp,euc-jp,default,latin
set foldmethod=marker
set clipboard=unnamedplus,autoselect
" コマンドライン補完
set wildmode=longest:full,full
set wildmenu

set ignorecase
set smartcase

" }}}
" Appearance {{{
set number "行番号表示
syntax on
"Tab、行末の半角スペースを明示的に表示する。
set list
"set listchars=tab:^\ ,trail:~
set listchars=tab:>\ ,trail:~,extends:>,precedes:< " 不可視文字の表示形式
set display=uhex  " 印字不可能文字を16進数で表示

set laststatus=2 "Always appear statusline
set cursorline "Highlight cursorline
"日本語の行の連結時には空白を入力しない。
"set formatoptions+=mM

"□や○の文字があってもカーソル位置がずれないようにする。
set ambiwidth=double
"画面最後の行をできる限り表示する。
set display+=lastline

" 80桁目をハイライト
set textwidth=0
if exists('&colorcolumn')
    set colorcolumn=+1
    autocmd FileType sh,c,cpp,perl,vim,ruby,python,haskell,scheme setlocal textwidth=80
endif
" }}}
" Plugins {{{
" NeoBundle {{{
" {{{
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Recommended to install
" After install, turn shell ~/.vim/bundle/vimproc, (n,g)make -f your_machines_makefile
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
NeoBundleLazy 'Shougo/unite.vim', {'autoload': {'commands': 'Unite'}}
NeoBundleLazy 'Shougo/vimfiler', { 'autoload':
      \ { 'commands': ['VimFiler', 'VimFilerExplorer'] }}
NeoBundleLazy 'Shougo/vimshell.vim', {'autoload':
      \ {'commands': ['VimShell']}}
NeoBundle 'supermomonga/vimshell-pure.vim', {'depends': 'Shougo/vimshell.vim'}
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'bouzuya/vim-ibus'
"NeoBundle 'fuenor/im_control.vim'
NeoBundle 'thinca/vim-template'
NeoBundle 'thinca/vim-singleton'
NeoBundle 'scrooloose/syntastic'
NeoBundleLazy 'thinca/vim-quickrun', {'autoload': {'commands': 'Quickrun'}}
NeoBundleLazy 'jcf/vim-latex', { 'autoload': { 'filetypes' : ['tex'] }}
NeoBundleLazy 'mrtazz/simplenote.vim', {'autoload': {'commands': 'Simplenote'}}
NeoBundleLazy 'mattn/emmet-vim', {'autoload': {'filetypes': ['html', 'css']}}
"NeoBundle 'superbrothers/vim-vimperator'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'vim-jp/vimdoc-ja'
"NeoBundleLazy 'skammer/vim-css-color', {'autoload': {'filetypes': 'css'}}
NeoBundleLazy 'lilydjwg/colorizer', {'autoload': {'filetypes': 'css'}}
NeoBundleLazy 'pangloss/vim-javascript', {'autoload':
      \ {'filetypes': 'javascript'}}
" Colorscheme
NeoBundle 'tomasr/molokai'
NeoBundle 'jpo/vim-railscasts-theme'
NeoBundle 'w0ng/vim-hybrid'
" ...

filetype plugin indent on     " Required!
"
" Brief help
" :NeoBundleList          - list configured bundles
" :NeoBundleInstall(!)    - install(update) bundles
" :NeoBundleClean(!)      - confirm(or auto-approve) removal of unused bundles

" Installation check.
NeoBundleCheck
""" }}}
" colorscheme {{{
set background=dark
set t_Co=256
"let g:solarized_termcolors=256
"let g:solarized_termtrans=1
"colorscheme solarized
"let g:hybrid_use_Xresources = 1
"colorscheme hybrid
"colorscheme railscasts

"let g:molokai_original = 1
"let g:rehash256 = 1
"colorscheme molokai
colorscheme hybrid
highlight Normal ctermbg=none
""" }}}
" neocomplcache {{{
"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
"" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
      \ 'default' : '',
      \ 'vimshell' : $HOME.'/.vimshell_hist',
      \ 'scheme' : $HOME.'/.gosh_completions'
      \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
     return neocomplcache#smart_close_popup() . "\<CR>"
     " For no inserting <CR> key.
     "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

" For cursor moving in insert mode(Not recommended)
"inoremap <expr><Left> neocomplcache#close_popup() . "\<Left>"
"inoremap <expr><Right> neocomplcache#close_popup() . "\<Right>"
"inoremap <expr><Up> neocomplcache#close_popup() . "\<Up>"
"inoremap <expr><Down> neocomplcache#close_popup() . "\<Down>"
" Or set this.
"let g:neocomplcache_enable_cursor_hold_i = 1
" Or set this.
let g:neocomplcache_enable_insert_char_pre = 1

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplcache_enable_auto_select = 1
"let g:neocomplcache_disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplcache_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
""" }}}
" vim-singleton {{{
call singleton#enable()
" }}}
" syntastic {{{
let g:syntastic_mode_map = {
    \  'mode': 'active',
    \ 'active_filetypes': ['c++'],
    \ 'passive_filetypes': []
    \ }
" }}}
" Vim-LaTeX {{{

filetype plugin on
set shellslash
set grepprg=grep\ -nH\ $*
filetype indent on
let g:tex_flavor='latex'
let g:Imap_UsePlaceHolders = 1
let g:Imap_DeleteEmptyPlaceHolders = 1
let g:Imap_StickyPlaceHolders = 0
let g:Tex_DefaultTargetFormat = 'pdf'
" dependency chain: .tex -> .pdf
let g:Tex_FormatDependency_pdf = 'pdf'
let g:Tex_FormatDependency_ps = 'ps'
"let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 -interaction=nonstopmode -file-line-error-style $*'
let g:Tex_CompileRule_pdf = 'xelatex -synctex=1 -interaction=nonstopmode $*'
let g:Tex_BibtexFlavor = 'pbibtex'
"let g:Tex_BibtexFlavor = 'upbibtex'
"let g:Tex_BibtexFlavor = 'bibtex'
"let g:Tex_BibtexFlavor = 'bibtexu'
let g:Tex_MakeIndexFlavor = 'mendex $*.idx'
"let g:Tex_MakeIndexFlavor = 'makeindex $*.idx'
"let g:Tex_MakeIndexFlavor = 'texindy $*.idx'
let g:Tex_UseEditorSettingInDVIViewer = 1
let g:Tex_ViewRule_pdf = 'evince'
"let g:Tex_ViewRule_pdf = 'okular --unique'
let g:Tex_ViewRule_ps = 'evince'
"let g:Tex_ViewRule_ps = 'okular --unique'
let g:Tex_ViewRule_dvi = 'xdvi -watchfile 1'
let g:Tex_IgnoredWarnings =
      \"Underfull\n".
      \"Overfull\n".
      \"specifier changed to\n".
      \"You have requested\n".
      \"Missing number, treated as zero.\n".
      \"There were undefined references\n".
      \"Citation %.%# undefined\n".
      \"Command %.%# invalid in math mode\n".
      \"Text page %.%# contains only floats\n"
      \"A float is stuck\n"
      \"Float too large\n"
      \"LaTeX Font Warning:"
      \"Label `' multiply defined."
      \"There were multiply-defined labels."

let g:Tex_IgnoreLevel = 12
let g:Tex_GotoError = 0
"}}}
"" im_control.vim {{{
"" IMの制御モード(5: pythonによるIBusの制御)
"let IM_CtrlMode=5
"" 「日本語入力固定モード」切替キー
"inoremap <silent> <C-\> <C-r>=IMState('FixMode')<CR>
"" PythonによるIBus制御指定
"let IM_CtrlIBusPython = 1
"" 「日本語入力固定モード」の全バッファローカルモード
"let IM_CtrlBufLocalMode = 1
"" GVimの時だけ「日本語入力固定モード」の vi協調モードを無効化
"let IM_vi_CooperativeMode = has('gui_running') ? 0 : 1
"" <ESC>押下後のIM切替開始までの反応が遅い場合はttimeoutlenを短く設定してみてください。
"set timeout timeoutlen=3000 ttimeoutlen=100
"" 制御スクリプトを非同期で呼び出し
"let IM_CtrlAsync = '&'
""" }}}
" ibus.vim {{{
" 挿入モードで <C-j> でIBusの有効無効を切り替える
inoremap <silent> <C-j> <C-\><C-o>:<C-u>call ibus#toggle()<CR>
" 挿入モードから <Esc> で抜ける際にIBusを無効にする
inoremap <silent> <Esc> <Esc>:<C-u>call ibus#disable()<CR>
" }}}
" Unite.vim {{{
" The prefix key.
nnoremap    [unite]   <Nop>
nmap    f [unite]

nnoremap <silent> [unite]c  :<C-u>UniteWithCurrentDir
        \ -buffer-name=files buffer file_mru bookmark file<CR>
nnoremap <silent> [unite]b  :<C-u>Unite buffer<CR>
"nnoremap <silent> [unite]b  :<C-u>UniteWithBufferDir
"        \ -buffer-name=files -prompt=%\  buffer file_mru bookmark file<CR>
nnoremap <silent> [unite]r  :<C-u>Unite file_mru<CR>
"nnoremap <silent> [unite]r  :<C-u>Unite
"        \ -buffer-name=register register<CR>
nnoremap <silent> [unite]o  :<C-u>Unite outline<CR>
nnoremap <silent> [unite]f
        \ :<C-u>Unite -buffer-name=resume resume<CR>
nnoremap <silent> [unite]d
        \ :<C-u>Unite -buffer-name=files -default-action=lcd directory_mru<CR>
nnoremap <silent> [unite]ma
        \ :<C-u>Unite mapping<CR>
nnoremap <silent> [unite]me
        \ :<C-u>Unite output:message<CR>
nnoremap  [unite]f  :<C-u>Unite source<CR>

nnoremap <silent> [unite]s
        \ :<C-u>Unite -buffer-name=files -no-split
        \ jump_point file_point buffer_tab
        \ file_rec:! file file/new file_mru<CR>

" Start insert.
"let g:unite_enable_start_insert = 1
"let g:unite_enable_short_source_names = 1

" To track long mru history.
"let g:unite_source_file_mru_long_limit = 3000
"let g:unite_source_directory_mru_long_limit = 3000

" Like ctrlp.vim settings.
"let g:unite_enable_start_insert = 1
"let g:unite_winheight = 10
"let g:unite_split_rule = 'botright'

" Prompt choices.
"let g:unite_prompt = '❫ '
"let g:unite_prompt = '» '

"autocmd FileType unite call s:unite_my_settings()
"function! s:unite_my_settings()"{{{
"  " Overwrite settings.
"
"  nmap <buffer> <ESC>      <Plug>(unite_exit)
"  imap <buffer> jj      <Plug>(unite_insert_leave)
"  "imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
"
"imap <buffer><expr> j unite#smart_map('j', '')
"imap <buffer> <TAB>   <Plug>(unite_select_next_line)
"imap <buffer> <C-w>     <Plug>(unite_delete_backward_path)
"imap <buffer> '     <Plug>(unite_quick_match_default_action)
"nmap <buffer> '     <Plug>(unite_quick_match_default_action)
"imap <buffer><expr> x
"        \ unite#smart_map('x', "\<Plug>(unite_quick_match_choose_action)")
"nmap <buffer> x     <Plug>(unite_quick_match_choose_action)
"nmap <buffer> <C-z>     <Plug>(unite_toggle_transpose_window)
"imap <buffer> <C-z>     <Plug>(unite_toggle_transpose_window)
"imap <buffer> <C-y>     <Plug>(unite_narrowing_path)
"nmap <buffer> <C-y>     <Plug>(unite_narrowing_path)
"nmap <buffer> <C-j>     <Plug>(unite_toggle_auto_preview)
"nmap <buffer> <C-r>     <Plug>(unite_narrowing_input_history)
"imap <buffer> <C-r>     <Plug>(unite_narrowing_input_history)
"nnoremap <silent><buffer><expr> l
"        \ unite#smart_map('l', unite#do_action('default'))
"
"let unite = unite#get_current_unite()
"if unite.buffer_name =~# '^search'
"  nnoremap <silent><buffer><expr> r     unite#do_action('replace')
"else
"  nnoremap <silent><buffer><expr> r     unite#do_action('rename')
"endif
"
"nnoremap <silent><buffer><expr> cd     unite#do_action('lcd')
"nnoremap <buffer><expr> S      unite#mappings#set_current_filters(
"        \ empty(unite#mappings#get_current_filters()) ? ['sorter_reverse'] : [])
"endfunction"}}}

let g:unite_source_file_mru_limit = 200
let g:unite_cursor_line_highlight = 'TabLineSel'
let g:unite_abbr_highlight = 'TabLine'

" For optimize.
let g:unite_source_file_mru_filename_format = ''

if executable('jvgrep')
  " For jvgrep.
  let g:unite_source_grep_command = 'jvgrep'
  let g:unite_source_grep_default_opts = '--exclude ''\.(git|svn|hg|bzr)'''
  let g:unite_source_grep_recursive_opt = '-R'
endif

	" For ack.
	if executable('ack-grep')
	  " let g:unite_source_grep_command = 'ack-grep'
	  " let g:unite_source_grep_default_opts = '--no-heading --no-color -a'
	  " let g:unite_source_grep_recursive_opt = ''
	endif
" }}}
" emmet-vim {{{
let g:user_emmet_settings = {'lang': 'ja'}
" }}}
" }}}
" keymapping {{{

" カーソルを表示行で移動
nnoremap j gj
nnoremap k gk
nnoremap <Space> <PageDown>

inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-p> <Up>
inoremap <C-n> <Down>
inoremap <C-a> <Home>
inoremap <C-e> <End>

" }}}
