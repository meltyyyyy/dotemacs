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
Bundle 'kevinw/pyflakes-vim'
Bundle 'Shougo/neocomplete.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'thinca/vim-quickrun'
Bundle 'kien/ctrlp.vim'
Bundle 'ivanov/vim-ipython'

" let vim run execute file
nnoremap <F11> :QuickRun<CR>
nnoremap <expr><silent> <C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "<C-c>"

" show file tree
map <F2> :NERDTreeToggle<CR>
