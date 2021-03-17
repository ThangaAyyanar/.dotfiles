
" Below function will automatically execute `syntax on` and `filetype plugin`
call plug#begin('~/.nvim/plugged')

" File explorer 
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'liuchengxu/vista.vim',{ 'on': 'Vista'}

" Nice status bar
Plug 'itchyny/lightline.vim'

" Color Scheme
Plug 'glepnir/zephyr-nvim'
Plug 'sainnhe/gruvbox-material'

" bulk commeter
Plug 'scrooloose/nerdcommenter'

" Syntax highlight
Plug 'rust-lang/rust.vim', {'for': 'rust' }
Plug 'keith/swift.vim', {'for': 'swift' }
Plug 'sheerun/vim-polyglot'
Plug 'urbit/hoon.vim', {'for': 'hoon'}
" Beancount
Plug 'nathangrigg/vim-beancount',{'for': 'beancount'}


" show file changes near the line number
Plug 'airblade/vim-gitgutter'

" Fuzzy finder
"Plug 'junegunn/fzf'
"Plug 'junegunn/fzf.vim'

" Emoji's
Plug 'junegunn/vim-emoji'

" git integration
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tommcdo/vim-fugitive-blame-ext'
"Plug 'stsewd/fzf-checkout.vim'

" Font icons for plugin
Plug 'ryanoasis/vim-devicons'

" start up screen for vim
Plug 'mhinz/vim-startify'

" tmux integration
Plug 'christoomey/vim-tmux-navigator'

" Auto pairs
"Plug 'jiangmiao/auto-pairs'
Plug 'Raimondi/delimitMate'

" Multiple cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Surround
Plug 'tpope/vim-surround'

"Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" Notes Management
Plug 'vimwiki/vimwiki', { 'for': 'markdown' }
Plug 'michal-h21/vim-zettel', { 'for': 'markdown' }
Plug 'godlygeek/tabular', { 'for': 'markdown' }
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  ,'for': 'markdown'}

" Jypter Notelike environment
"Plug 'jupyter-vim/jupyter-vim'
Plug 'metakirby5/codi.vim', { 'on':  'Codi' }
"Plug 'jpalardy/vim-slime'

" css colors
" you need to type :ColorHighlight to activate this plugin
Plug 'chrisbra/Colorizer'

Plug 'RRethy/vim-illuminate'

" easily navigate between quickfix,buffers and more
Plug 'tpope/vim-unimpaired'

" Smooth scrolling
Plug 'psliwka/vim-smoothie'

" Language Server Protcol
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
Plug 'steelsojka/completion-buffers'

Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Neovim Tree shitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'

" Debugging plugin
Plug 'nvim-telescope/telescope-dap.nvim'
Plug 'mfussenegger/nvim-dap'
Plug 'mfussenegger/nvim-dap-python'

" Testing plugin
Plug 'vim-test/vim-test'

" Rest API client
Plug 'baverman/vial', {'for': 'vial-http'}
Plug 'ThangaAyyanar/vial-http', {'for': 'vial-http'}

"GnuGPG
Plug 'jamessan/vim-gnupg'

" Undotree
Plug 'mbbill/undotree', { 'on':  'UndotreeToggle' }

"Cheat.sh integration
Plug 'dbeniamine/cheat.sh-vim'
call plug#end()

" Color Scheme
"let g:molokai_original = 1
"colorscheme molokai
colorscheme gruvbox-material
autocmd BufEnter *.md colorscheme zephyr

" leader key and it's bindings

let mapleader = ","

nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

nnoremap <leader>a :NERDTreeToggle<cr>
nnoremap <leader>wgt :VimwikiRebuildTags!<cr>:VimwikiGenerateTagLinks<cr><c-l>
nnoremap <leader><space> :nohlsearch<cr>
nnoremap <leader>t :Vista!!<cr>
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <leader>j V:!jq<cr>:set filetype=json<cr>
nnoremap <leader>x V:!xmllint --format -<cr>:set filetype=xml<cr>
nnoremap <leader>ac vipyPgvO<Esc>O<Esc>gv:!curl --config -<CR>
" Close all folds and open and focus on fold containing current line
nnoremap <Leader>z zMzvzz
" Map to use marker folding
nnoremap <silent> <Leader>mf :set foldmethod=marker<CR>zv
" Change quote and back tick for easy navigation to marks
noremap ' `

" Sync with os clipboard
vnoremap <leader>y "*y
nnoremap <leader>y "*y
nnoremap <leader>p "*p

vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

nnoremap <leader>gg :diffget<cr>
nnoremap <leader>gf :diffget //2<cr>
nnoremap <leader>gh :diffput //3<cr>
nnoremap <leader>gs :G<cr>

"copy current path
nnoremap <leader>cp :let @" = expand("%")<cr>

" Paste system clipboard to command line
cnoremap <A-p> <C-R>"

" dv - on :G to resolve

" Telescope.nvim mapping
nnoremap <leader>ps :lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For > ")})<CR>
nnoremap <C-p> :lua require('telescope.builtin').git_files()<CR>
nnoremap <Leader>pf :lua require('telescope.builtin').find_files()<CR>

nnoremap <leader>pw :lua require('telescope.builtin').grep_string { search = vim.fn.expand("<cword>") }<CR>

" Treesitter syntax highlight
lua require'nvim-treesitter.configs'.setup { highlight = { enable = true } }

set completeopt=menuone,noinsert,noselect

" Lsp configurations
nnoremap <leader>vd :lua vim.lsp.buf.definition()<CR>
nnoremap <leader>vi :lua vim.lsp.buf.implementation()<CR>
nnoremap <leader>vsh :lua vim.lsp.buf.signature_help()<CR>
nnoremap <leader>vrr :lua vim.lsp.buf.references()<CR>
nnoremap <leader>vrn :lua vim.lsp.buf.rename()<CR>
nnoremap <leader>vh :lua vim.lsp.buf.hover()<CR>
nnoremap <leader>vca :lua vim.lsp.buf.code_action()<CR>
nnoremap <leader>vsd :lua vim.lsp.util.show_line_diagnostics(); vim.lsp.util.show_line_diagnostics()<CR>

let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
lua require'lspconfig'.pyright.setup{ on_attach=require'completion'.on_attach }
lua require'lspconfig'.sourcekit.setup{ on_attach=require'completion'.on_attach }
lua require'lspconfig'.vimls.setup{ on_attach=require'completion'.on_attach }
lua require'lspconfig'.dartls.setup{ on_attach=require'completion'.on_attach }

autocmd BufEnter * lua require'completion'.on_attach()

let g:completion_chain_complete_list = [
    \{'complete_items': ['lsp', 'snippet', 'buffers']},
    \{'mode': '<c-p>'},
    \{'mode': '<c-n>'}
\]

" Ultisnips in autocompletion
let g:completion_enable_snippet = 'UltiSnips'

" Debugging Initialization
lua require('telescope').load_extension('dap')
lua require('dap-python').setup('/usr/bin/python3')

" Debugging
nnoremap <silent> <F5> :lua require'dap'.continue()<CR>
nnoremap <silent> <leader>dd :lua require('dap').continue()<CR>
nnoremap <silent> <F10> :lua require'dap'.step_over()<CR>
nnoremap <silent> <F11> :lua require'dap'.step_into()<CR>
nnoremap <silent> <F12> :lua require'dap'.step_out()<CR>
nnoremap <silent> <leader>b :lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <leader>B :lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
nnoremap <silent> <leader>lp :lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>
nnoremap <silent> <leader>dr :lua require'dap'.repl.open()<CR>
nnoremap <silent> <leader>dl :lua require'dap'.repl.run_last()<CR>`

" Python specific debugging
nnoremap <silent> <leader>dn :lua require('dap-python').test_method()<CR>
vnoremap <silent> <leader>ds <ESC>:lua require('dap-python').debug_selection()<CR>

nnoremap <silent> <leader>tn :TestNearest<CR>
"nnoremap <silent> t<C-f> :TestFile<CR>
"nnoremap <silent> t<C-s> :TestSuite<CR>
"nnoremap <silent> t<C-l> :TestLast<CR>
"nnoremap <silent> t<C-g> :TestVisit<CR>

let test#strategy = "neovim"
let test#neovim#term_position = "bot"

set cursorline
set number
set relativenumber
set undofile
set ignorecase
set termguicolors
set nostartofline
set nojoinspaces
set noswapfile
set scrolloff=10
set sidescrolloff=10
set nowrap

" Don't update the display while executing macros
set lazyredraw

set undodir=/tmp
" not interfere with tmux scroll
set mouse=a
" clipboard sharing - checked in mac
"set clipboard=unnamed
" spell checking
"set spell
"live subtitution
set inccommand=nosplit

"better verical movement - particullary in long sentence in line
nnoremap j gj
nnoremap k gk

" move between split easily
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-h> <c-w>h
map <c-l> <c-w>l
map <c-q> <c-w>q
tnoremap <c-h> <C-\><C-N><C-w>h
tnoremap <c-j> <C-\><C-N><C-w>j
tnoremap <c-k> <C-\><C-N><C-w>k
tnoremap <c-l> <C-\><C-N><C-w>l

" display hidden characters
set list
set listchars=tab:▸\ ,eol:¬

" tab options
set shiftwidth=2
set tabstop=2
set softtabstop=2
set expandtab
set smarttab

"set fileencoding=utf8

"Set default grep to rg
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

"Python
let g:python3_host_prog = '/usr/bin/python3'

" quickly insert a timestamp
nnoremap tt "=strftime("%Y-%m-%d")<cr>p

" enable vim sign column
set signcolumn=yes

" column limit
"set textwidth=80
set colorcolumn=81

set splitbelow
set splitright

"terminal remap
tnoremap <C-[> <C-\><C-n>

" use <++> as placeholder - from luke vim script
inoremap <leader><leader> <Esc>/<++><Enter>"_c4l
nnoremap <leader><leader> <Esc>/<++><Enter>"_c4l

"foldding the content
set foldlevel=1
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
highlight Folded guifg=PeachPuff4

" Terminal Enter and close
function Terminal_enter()
    set relativenumber!
    set number!
    set signcolumn=no
    startinsert
endfunction
function Terminal_close()
    set relativenumber
    set number
    set signcolumn=yes
endfunction

autocmd TermOpen * :call Terminal_enter()
autocmd TermClose * :call Terminal_close()

" Copy matches which corresponds to search
function! CopyMatches(reg)
  let hits = []
  %s//\=len(add(hits, submatch(0))) ? submatch(0) : ''/gne
  let reg = empty(a:reg) ? '+' : a:reg
  execute 'let @'.reg.' = join(hits, "\n") . "\n"'
endfunction
command! -register CopyMatches call CopyMatches(<q-reg>)

" Lightline Configuration
function! LightlineGitGutter()
  if !get(g:, 'gitgutter_enabled', 0) || empty(FugitiveHead())
    return ''
  endif
  let [ l:added, l:modified, l:removed ] = GitGutterGetHunkSummary()
  return printf('+%d ~%d -%d', l:added, l:modified, l:removed)
endfunction

let g:lightline = {
      \ 'mode_map': {
      \ 'n' : 'N',
      \ 'i' : 'I',
      \ 'R' : 'R',
      \ 'v' : 'V',
      \ 'V' : 'VL',
      \ "\<C-v>": 'VB',
      \ 'c' : 'C',
      \ 's' : 'S',
      \ 'S' : 'SL',
      \ "\<C-s>": 'SB',
      \ 't': 'T',
      \ },
      \ 'colorscheme': 'one',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'githunk','gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'githunk': 'LightlineGitGutter',
      \   'gitbranch': 'fugitive#head',
      \ },
      \ }

" Apply colors for filename in Lightline inactive state 
" Excellent for editing files horizontally
autocmd VimEnter * call SetupLightlineColors()
function SetupLightlineColors() abort
  let l:palette = lightline#palette()
  let l:palette.inactive.left = [["#282c34","#ff8c66",235,168,"bold"],["#abb2bf","#3e4452",145,240]]
  call lightline#colorscheme()
endfunction

" Vista - Tagbar
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista_executive_for = {
  \ 'dart': 'vim_lsp',
  \ }

" Vim wiki
let g:vimwiki_global_ext = 0
let g:vimwiki_list = [{'path': '~/Documents/My Library','auto_diary_index': 1,'syntax': 'markdown','ext': '.md'},{"path":"/Users/thanga-6745/Zoho\ WorkDrive\ \(Enterprise\)/My\ Folders/SlipBox", 'auto_tags': 1, 'auto_toc': 1,'syntax': 'markdown','ext': '.md'}]
let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}

"vim-zettel
let g:zettel_format = '%Y%m%d%H%M-%S'
let g:zettel_options = [{},{"front_matter" : {"tags" : ""}, "template" :  "~/Templates/zettel.tpl"}]
let g:zettel_fzf_command = "rg --column --line-number --ignore-case --no-heading --color=always "
nnoremap <leader>vt :VimwikiSearchTags<space>
nnoremap <leader>vs :VimwikiSearch<space>
nnoremap <leader>gt :VimwikiRebuildTags!<cr>:ZettelGenerateTags<cr><c-l>
nnoremap <leader>zl :ZettelSearch<cr>
nnoremap <leader>zn :ZettelNew<cr><cr>:4d<cr>:w<cr>ggA
nnoremap <leader>bl :VimwikiBacklinks<cr>


" git gutter setups
let g:gitgutter_sign_added = emoji#for('small_blue_diamond')
let g:gitgutter_sign_modified = emoji#for('small_orange_diamond')
let g:gitgutter_sign_removed = emoji#for('small_red_triangle')
let g:gitgutter_sign_modified_removed = emoji#for('collision')

" TEMPLATES
" Blog Template
iabbrev bpfm +++<cr>title = "<++>"<cr>date = <++><cr>description = "<++>"<cr>in_search_index = true<cr><cr>[taxonomies]<cr>categories = ["<++>"]<cr>tags = ["<++>"]<cr>authors = ["<++>"]<cr>+++<cr><cr>
" Task warrior template
iabbrev twnt task add "<++>" project:<++> +<++><cr>task +LATEST annotate "<++>"

" User Defined function
function TodoTaskAdd()
    "https://coderwall.com/p/auy6fa/vim-get-current-file-path
    let annotation = expand('%:p')
    let task = expand("%:t")
    let lineNumber = line(".")
    execute '!task rc.data.location=~/TaskBase/Office add '.task.' +Todo project:Remainder'
    execute '!task rc.data.location=~/TaskBase/Office +LATEST annotate "file:'.annotation.':'.lineNumber.'"'
endfunction

function GitFileDiff(msg)
  Gvdiffsplit "'.msg.':%"
endfunction

:command -nargs=1 -complete=customlist,fugitive#EditComplete Gfilediff call GitFileDiff(<q-args>)

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

let g:codi#virtual_text = 0
let g:codi#rightalign = 0
let g:codi#interpreters = {
                   \ 'python': {
                       \ 'bin': '/usr/bin/python3',
                       \ },
                   \ }

