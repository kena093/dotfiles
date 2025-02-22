" 全般設定

set number          " 行番号を表示
set smartindent     " インテリジェントなインデント
set nobackup       " バックアップファイルを作成しない
set noswapfile     " スワップファイルを作成しない
set autoread        " ファイルが外部で変更されたら自動的に読み込む
set showmode        " 現在のモード（INSERT, VISUALなど）を表示
set showcmd         " 入力したコマンドを表示
set cursorline      " 現在の行をハイライト表示
set incsearch       " インクリメンタルサーチ（入力中に検索）
set hlsearch        " 検索結果をハイライト表示
inoremap <silent> jj <ESC>  " INSERTモードで `jj` を押すと ESC と同じ (INSERTモードから抜けるショートカット)
set nocompatible    " Vim の機能を最大限に活用（Vi互換モードをオフ）
set encoding=utf-8  " ファイルのエンコーディングを UTF-8 に設定
set fileencodings=utf-8,cp932,euc-jp,sjis " ファイルエンコーディングの自動判別順序
set clipboard=unnamed,unnamedplus " システムクリップボードを共有（`+` レジスタを使用）
set expandtab       " タブをスペースに展開
set ignorecase      " 検索時に大文字・小文字を区別しない
set smartcase       " 検索語に大文字が含まれている場合は区別する
set wrapscan        " ファイルの最後まで検索したら先頭に戻る
set showmatch       " 対応する括弧をハイライト表示
set autoindent      " 自動インデント
syntax on           " シンタックスハイライトを有効化
set backspace=indent,eol,start " Backspaceキーでインデント、行末、挿入開始位置まで削除可能
set wildignore+=*.pdf,*.o,*.obj,*.jpg,*.png " ファイル検索時に無視するファイルパターン
set splitbelow      " 新しいウィンドウを下に分割
set splitright      " 新しいウィンドウを右に分割
set tabstop=4       " タブ幅を4スペースに設定
set mouse=a         " マウス操作を有効化（全て）
set guicursor=i:block "GUI環境でのカーソル形状をブロックに変更 (INSERTモード時)

" ステータスライン設定

set statusline=%F " ファイル名を表示
set statusline+=%m " 変更されたかどうかを表示
set statusline+=%r " 読み取り専用かどうかを表示
set statusline+=%h " ヘルプファイルかどうかを表示
set statusline+=%w " プレビューウィンドウかどうかを表示
set statusline+=:%l " 現在の行番号を表示
set statusline+=%= " ステータスラインの右側を開始
set statusline+=\ %Y[%{&fileencoding}] " ファイルエンコーディングを表示
"set statusline+=%{coc#status()} "  (コメントアウト) coc.nvim のステータスを表示。coc.nvim を使用していない場合は削除またはコメントアウト
set laststatus=2 " ステータスラインを常に表示 (0:表示しない, 1:2つ以上のウィンドウがある場合のみ, 2:常に表示)
set list         " 不可視文字を表示 (タブ、行末など)

" キーマッピング

nnoremap <Return><Return> <c-w><c-w>  " Enterを2回押すとウィンドウを移動
nnoremap confe :e $MYVIMRC<CR>       " `confe` を押すと vimrc を開く
nnoremap confr :source $MYVIMRC<CR>   " `confr` を押すと vimrc を再読み込み

" ファイルタイプ別の設定

au BufNewFile,BufRead *.c set filetype=c  " *.c ファイルを C として認識
au BufNewFile,BufRead *.cpp set filetype=cpp " *.cpp ファイルを C++ として認識
au BufNewFile,BufRead *.cc set filetype=cpp  " *.cc ファイルを C++ として認識
au BufNewFile,BufRead *.h set filetype=cpp   " *.h ファイルを C++ として認識
au BufNewFile,BufRead *.ejs set filetype=html  " *.ejs ファイルを HTML として認識
au BufNewFile,BufRead *.satyh set filetype=satysfi " *.satyh ファイルを Satysfi として認識
au BufNewFile,BufRead *.pl set filetype  " *.pl ファイルを自動判別

" ファイルタイプ固有の設定 (インデントなど)

" autocmd FileType go AutoFormatBuffer gofmt  " (コメントアウト) Go ファイルを gofmt で自動フォーマット。関数`AutoFormatBuffer`の定義がないため、コメントアウト
au FileType c set tabstop=2    " C ファイルのタブ幅を 2 スペースに設定
au FileType c set shiftwidth=2 " C ファイルのインデント幅を 2 スペースに設定
au FileType c set expandtab   " C ファイルでタブをスペースに展開

au FileType markdown set tabstop=2    " Markdown ファイルのタブ幅を 2 スペースに設定
au FileType markdown set shiftwidth=2 " Markdown ファイルのインデント幅を 2 スペースに設定
au FileType markdown set expandtab   " Markdown ファイルでタブをスペースに展開

au FileType json set tabstop=2    " JSON ファイルのタブ幅を 2 スペースに設定
au FileType json set shiftwidth=2 " JSON ファイルのインデント幅を 2 スペースに設定
au FileType json set expandtab   " JSON ファイルでタブをスペースに展開
au FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>  " (コメントアウト) JSON ファイルで Ctrl-F を押すと JsonBeautify() 関数を実行。関数`JsonBeautify`の定義がないため、コメントアウト

call plug#begin('~/.vim/plugged')
Plug 'bronson/vim-trailing-whitespace'
" rust用のプラグイン
Plug 'rust-lang/rust.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

syntax enable
filetype plugin indent on

" 保存時に自動でrustfmt
let g:rustfmt_autosave = 1


set updatetime=300

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
