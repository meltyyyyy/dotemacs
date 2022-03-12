" My confguration for vim
" run this command before using vim
"
" git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim
" mv Vundle.vim vundle
" vim +PluginInstall +qall

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'
Bundle 'davidhalter/jedi-vim'
Bundle 'bronson/vim-trailing-whitespace'
Bundle 'airblade/vim-gitgutter'
Bundle 'Flake8-vim'
Bundle 'hynek/vim-python-pep8-indent'
Bundle 'scrooloose/syntastic'
Bundle 'Shougo/neocomplete.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'thinca/vim-quickrun'
Bundle 'kien/ctrlp.vim'
Bundle 'ivanov/vim-ipython'
Bundle 'neomake/neomake'

" let vim run execute file
nnoremap <F11> :QuickRun<CR>
nnoremap <expr><silent> <C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "<C-c>"

" show file tree
map <F2> :NERDTreeToggle<CR>

"------------------------------------
" neomake
"------------------------------------
call neomake#config#set('ft.python.pylama.exe', 'pylava')
call neomake#configure#automake('nrwi', 100)
let g:neomake_open_list = 2
map <C-n> :lnext<CR>
map <C-m> :lprev<CR>

"------------------------------------
" pyflakes
"------------------------------------
let g:PyFlakeOnWrite = 1
let g:PyFlakeCheckers = 'pep8,mccabe,pyflakes'
let g:PyFlakeDefaultComplexity=10

"------------------------------------
" syntastic
"------------------------------------
let g:syntastic_python_checkers = ['pyflakes', 'pep8']

"------------------------------------
" neocomplete.vim & jedi.vim
"------------------------------------
autocmd FileType python setlocal completeopt-=preview
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0

if !exists('g:neocomplete#force_omni_input_patterns')
        let g:neocomplete#force_omni_input_patterns = {}
endif

" let g:neocomplete#force_omni_input_patterns.python = '\%([^.\t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
let g:neocomplete#force_omni_input_patterns.python = '\h\w*\|[^. \t]\.\w*'

"------------------------------------
" neocomplete.vim
"------------------------------------
"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  " return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
   return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()

" Close popup by <Space>.
inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"

" autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

" basic configuration
set number
set encoding=utf-8
set laststatus=2
set autoindent
syntax on
set background=light
set shiftwidth=4
if has("mouse") " Enable the use of the mouse in all modes
  set mouse=a
endif

