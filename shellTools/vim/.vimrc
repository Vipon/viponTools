" Set encodings
scriptencoding utf-8
set encoding=utf-8

" Select color for syntax style
colorscheme peachpuff

" Enable syntax highlighting
syntax on

" 'hidden' option allows to hide old files insted of close them, when you open
" new file in the same window. It permits to return to the old file after it
" and undo some changes.
set hidden

" Better command-line completion
set wildmenu

" Show partial commands in the last line of the screen
set showcmd

" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent

" Display the cursor position on the last line of the screen or in the status
" line of a window
set ruler

" Always display the status line, even if only one window is displayed
set laststatus=2

" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm

" Use visual bell instead of beeping when doing something wrong
set visualbell

" And reset the terminal code for the visual bell. If visualbell is set, and
" this line is also included, vim will neither flash nor beep. If visualbell
" is unset, this does nothing.
set t_vb=

" Enable use of the mouse for all modes
set mouse=a

" Set the command window height to 2 lines, to avoid many cases of having to
" "press <Enter> to continue"
set cmdheight=2

" Display line numbers on the left
set number

" enable filetype detection:
filetype on
filetype plugin on
filetype indent on " file type based indentation

" Indentation settings for using 4 spaces instead of tabs.
autocmd FileType c,cpp,sh,java,txt,idl,cdl,edl set shiftwidth=4 softtabstop=4 expandtab
autocmd FileType haskell,cmake,vim,psl,yaml set shiftwidth=2 softtabstop=2 expandtab
autocmd FileType simgen set shiftwidth=4 softtabstop=4 expandtab

" Add check spelling for gitcommit
autocmd FileType gitcommit setlocal spell

" Show tabs and spaces.
set listchars=tab:>-,nbsp:·,space:·
set list

" Aout removing useless whitespaces at the end of lines.
autocmd BufWritePre * %s/\s\+$//e

" Vim-plug commands
let PLUGIN_DIR = $HOME . '/.vim/plugins/'

" Plugins will be downloaded under the specified directory.
call plug#begin(PLUGIN_DIR)

" Declare the list of plugins.
Plug 'terryma/vim-multiple-cursors' " multiple-cursors like Sublime

" Language support
Plug 'neovimhaskell/haskell-vim'
Plug 'octol/vim-cpp-enhanced-highlight'

" Git
Plug 'airblade/vim-gitgutter'   " show line status
Plug 'rhysd/git-messenger.vim'  " show blame
Plug 'tpope/vim-fugitive'       " show branch + cmds

" LSP
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

" Autocompletion.
" asyncomplete has conflict with coc.nvim.
" Plug 'prabirshrestha/asyncomplete.vim'
" Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" List ends here. Plugins become visible to Vim after this call.
call plug#end()

"if executable('ccls')
"  au User lsp_setup call lsp#register_server({
"    \ 'name': 'ccls',
"    \ 'cmd': {server_info->['ccls']},
"    \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'compile_commands.json'))},
"    \ 'initialization_options': {'cache': {'directory': '/tmp/ccls/cache' }},
"    \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
"    \ })
"endif
if executable('clangd')
  au User lsp_setup call lsp#register_server({
    \ 'name': 'clangd',
    \ 'cmd': {server_info->['clangd', '-background-index']},
    \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
    \ })
endif

if executable('pyls')
  au User lsp_setup call lsp#register_server({
    \ 'name': 'pyls',
    \ 'cmd': {server_info->['pyls']},
    \ 'whitelist': ['python'],
    \ })
endif

" Close annoying  and excess LSP SignatureHelp
let g:lsp_signature_help_enabled = 0

