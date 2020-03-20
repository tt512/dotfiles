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
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'Shougo/defx.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'kristijanhusak/defx-git'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-startify'
Plug 'tyru/eskk.vim'
Plug 'ludovicchabant/vim-gutentags'
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
Plug 'morhetz/gruvbox'
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

" Some langservers have issues with backup files
" see https://github.com/neoclide/coc.nvim/issues/649
set nobackup
set nowritebackup

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

" Default indent level
set expandtab tabstop=4 shiftwidth=4 softtabstop=4

" Indent level for specific filetypes
au vimrc FileType html,javascript,typescript,javascriptreact,typescriptreact,pascal,ruby,yaml,zsh,vim,cucumber setl et ts=2 sw=2 sts=2
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
" coc {{{2
let g:coc_global_extensions = [
      \ 'coc-lists',
      \ 'coc-git',
      \ 'coc-json',
      \ 'coc-tsserver',
      \ 'coc-eslint',
      \ 'coc-prettier',
      \ 'coc-python']

" Highlight symbol under cursor on CursorHold
autocmd vimrc CursorHold * silent call CocActionAsync('highlight')

" Setup formatexpr specified filetype(s).
autocmd vimrc FileType typescript,json setl formatexpr=CocAction('formatSelected')
" Update signature help on jump placeholder
autocmd vimrc User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')
" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)
" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Completion keybindings
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" function for documentation popup
function! s:coc_show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
" }}}
" eskk {{{2
let g:eskk#large_dictionary = '~/SKK-JISYO.XL'
" }}}
" Startify {{{2
let g:startify_change_to_dir = 0
" }}}
" Defx {{{2
call defx#custom#option('_', {
      \ 'root_marker': ':',
      \ 'split': 'vertical',
      \ 'direction': 'topleft',
      \ 'winwidth': 30,
      \ 'columns': 'indent:icon:filename:git',
      \ })
call defx#custom#column('filename', {
      \ 'root_marker_highlight': 'Ignore',
      \ 'min_width': 30,
      \ 'max_width': 30,
      \ })
call defx#custom#column('icon', {
      \ 'directory_icon': '▶',
      \ 'opened_icon': '▼',
      \ 'root_icon': '-',
      \ })
call defx#custom#column('git', 'indicators', {
      \ 'Modified'  : 'M',
      \ 'Staged'    : 'S',
      \ 'Untracked' : 'U',
      \ 'Renamed'   : 'R',
      \ 'Unmerged'  : '═',
      \ 'Ignored'   : 'I',
      \ 'Deleted'   : 'D',
      \ 'Unknown'   : '?'
      \ })
autocmd vimrc FileType defx call s:defx_my_settings()
function! s:defx_my_settings() abort
  " Define mappings
  nnoremap <silent><buffer><expr> <CR>
              \ defx#is_directory() ?
              \ defx#do_action('open_or_close_tree') :
              \ defx#do_action('drop')
  nnoremap <silent><buffer><expr> l defx#do_action('open_tree')
  nnoremap <silent><buffer><expr> h defx#do_action('close_tree')
  nnoremap <silent><buffer><expr> c defx#do_action('copy')
  nnoremap <silent><buffer><expr> m defx#do_action('move')
  nnoremap <silent><buffer><expr> p defx#do_action('paste')
  nnoremap <silent><buffer><expr> r defx#do_action('rename')
  nnoremap <silent><buffer><expr> d defx#do_action('remove')
  nnoremap <silent><buffer><expr> K defx#do_action('new_directory')
  nnoremap <silent><buffer><expr> N defx#do_action('new_file')
  nnoremap <silent><buffer><expr> M defx#do_action('new_multiple_files')
  nnoremap <silent><buffer><expr> I defx#do_action('toggle_ignored_files')
  nnoremap <silent><buffer><expr> q defx#do_action('quit')
  nnoremap <silent><buffer><expr> cd defx#do_action('change_vim_cwd')
endfunction
" }}}
" }}}
" Key Mappings {{{1
" EasyAlign
" For visual mode (e.g. vip<Enter>)
vmap <Enter>   <Plug>(EasyAlign)
" For normal mode, with Vim movement (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

" CoC

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Remap for rename current word
nmap <silent>gn <Plug>(coc-rename)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>coc_show_documentation()<CR>

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <C-d> <Plug>(coc-range-select)
xmap <silent> <C-d> <Plug>(coc-range-select)

" Using CocList
nnoremap <silent> <space>a  :<C-u>CocList actions<cr>
nnoremap <silent> <space>b  :<C-u>CocList buffers<cr>
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
nnoremap <silent> <space>d  :<C-u>CocList diagnostics<cr>
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
nnoremap <silent> <space>f  :<C-u>CocList files<cr>
nnoremap <silent> <space>g  :<C-u>CocList --normal gstatus<cr>
nnoremap <silent> <space>l  :<C-u>CocList lines<cr>
nnoremap <silent> <space>L  :<C-u>CocList lists<cr>
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
nnoremap <silent> <space>h  :<C-u>CocList helptags<cr>
nnoremap <silent> <space>r  :<C-u>CocList -A mru<cr>
nnoremap <silent> <space>m  :<C-u>CocList marks<cr>
nnoremap <silent> <space>M  :<C-u>CocList maps<cr>
nnoremap <silent> <space>s  :<C-u>CocList searchhistory<cr>
nnoremap <silent> <space>S  :<C-u>CocList -I symbols<cr>
nnoremap <silent> <space>v  :<C-u>CocList vimcommands<cr>
nnoremap <silent> <space>V  :<C-u>CocList cmdhistory<cr>
nnoremap <silent> <space>w  :<C-u>CocList windows<cr>
nnoremap <silent> <space>q  :<C-u>CocList quickfix<cr>
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

nnoremap <silent> <space>t  :<C-u>Defx<CR>
nnoremap <silent> <space>T  :<C-u>Defx -toggle<CR>
nnoremap <silent> <space>m  :<C-u>TagbarOpen<CR>
nnoremap <silent> <space>M  :<C-u>TagbarClose<CR>

nnoremap <silent> <space>R :<C-u>so $MYVIMRC<cr>:echo 'vimrc reloaded'<cr>
nnoremap <silent> <space>E :<C-u>e $MYVIMRC<cr>

" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" }}}
