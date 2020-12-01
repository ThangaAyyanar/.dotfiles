
" Below function will automatically execute `syntax on` and `filetype plugin`
call plug#begin('~/.nvim/plugged')

" File explorer 
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'majutsushi/tagbar', { 'on':  'TagbarToggle' }

" Nice status bar
Plug 'itchyny/lightline.vim'

" Color Scheme
Plug 'morhetz/gruvbox'
Plug 'tomasr/molokai'
"Plug 'junegunn/seoul256.vim'

" bulk commeter
Plug 'scrooloose/nerdcommenter'

" Syntax highlight
Plug 'rust-lang/rust.vim', {'for': 'rust' }
Plug 'keith/swift.vim', {'for': 'swift' }
Plug 'sheerun/vim-polyglot'
Plug 'urbit/hoon.vim'

" show file changes near the line number
Plug 'airblade/vim-gitgutter'

" Fuzzy finder
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

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

" Goyo integration - distraction free writing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'junegunn/limelight.vim', { 'on': 'Goyo' }

" Auto pairs
Plug 'jiangmiao/auto-pairs'

" Multiple cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Surround
Plug 'tpope/vim-surround'

" Auto completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

"Snippets
Plug 'honza/vim-snippets'

" Notes Management
Plug 'vimwiki/vimwiki', { 'for': 'markdown' }
Plug 'michal-h21/vim-zettel', { 'for': 'markdown' }
Plug 'godlygeek/tabular'

" Jypter Notelike environment
"Plug 'jupyter-vim/jupyter-vim'
Plug 'metakirby5/codi.vim', { 'on':  'Codi' }
"Plug 'jpalardy/vim-slime'

" css colors
Plug 'chrisbra/Colorizer'

Plug 'RRethy/vim-illuminate'

" zoom in zoom out window
" also check https://vim.fandom.com/wiki/Window_zooming_convenience
Plug 'dhruvasagar/vim-zoom'
" easily navigate between quickfix,buffers and more
Plug 'tpope/vim-unimpaired'

" Smooth scrolling
Plug 'psliwka/vim-smoothie'

" 0.5 features
"Plug 'neovim/nvim-lspconfig'
"Plug 'nvim-lua/completion-nvim'

call plug#end()

" Color Scheme
"let g:molokai_original = 1
"colorscheme molokai
colorscheme gruvbox

" leader key and it's bindings

let mapleader = ","

nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

nnoremap <leader>a :NERDTreeToggle<cr>
nnoremap <leader><space> :nohlsearch<cr>
nnoremap <leader>t :TagbarToggle<cr>
nnoremap <leader>m :Goyo<cr>
nnoremap <leader>j V:!jq<cr>:set filetype=json<cr>
nnoremap <leader>x V:!xmllint --format -<cr>:set filetype=xml<cr>
nnoremap <leader>cc vipyPgvO<Esc>O<Esc>gv:!curl --config -<CR>

" Sync with os clipboard
vnoremap <leader>y "*y
nnoremap <leader>y "*y
nnoremap <leader>p "*p

vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

map <leader>- :<c-u>split<cr>
map <leader>\ :<c-u>vsplit<cr>

nnoremap <leader>gg :diffget<cr>
nnoremap <leader>gf :diffget //2<cr>
nnoremap <leader>gh :diffput //3<cr>
nnoremap <leader>gs :G<cr>
" dv - on :G to resolve

" Global file search from root directory - ferret

set cursorline
set number
set relativenumber
set undofile
set ignorecase
set termguicolors
set nostartofline
set nojoinspaces
set noswapfile

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

" fzf
if executable('fzf')
    " All commands provided by fzf will have this prefix
    let g:fzf_command_prefix = 'Fzf'
    " Border color
    let g:fzf_layout = {'up':'~90%', 'window': { 'width': 0.8, 'height': 0.8,'yoffset':0.5,'xoffset': 0.5, 'highlight': 'Todo', 'border': 'sharp' } }
    let $FZF_DEFAULT_OPTS = '--layout=reverse --info=inline'
    let $FZF_DEFAULT_COMMAND="rg --files --hidden" 
    nnoremap <c-p> :FzfFiles<cr>
    nnoremap <space><space> :FzfBuffers<cr>
    nnoremap <leader>f :FzfRg<cr>
    nnoremap <leader>g :FzfGitFiles<cr>

    "FZF git checkout plugin
    nnoremap <leader>gc :FzfGCheckout<cr>
endif

" quickly insert a timestamp
nnoremap tt "=strftime("%d %b %y %x")<cr>p

" enable vim sign column
set signcolumn=yes

" column limit
"set textwidth=80
set colorcolumn=80

set splitbelow
set splitright

"terminal remap
tnoremap <C-[> <C-\><C-n>

" use <++> as placeholder - from luke vim script
inoremap <leader><leader> <Esc>/<++><Enter>"_c4l
nnoremap <leader><leader> <Esc>/<++><Enter>"_c4l

"foldding the content
set foldmethod=indent
set foldlevel=1
highlight Folded guifg=PeachPuff4

" Goyo Configuration
function! s:goyo_enter()
    colorscheme gruvbox
    Limelight
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave Limelight!

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
      \             [ 'githunk','gitbranch', 'cocstatus', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'githunk': 'LightlineGitGutter',
      \   'gitbranch': 'fugitive#head',
      \   'cocstatus': 'coc#status'
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

" Vim wiki
let g:vimwiki_list = [{'path': '~/Documents/My Library','syntax': 'markdown','ext': '.md'},{"path":"/Users/thanga-6745/Zoho\ WorkDrive\ \(Enterprise\)/My\ Folders/SlipBox", 'auto_tags': 1, 'auto_toc': 1,'syntax': 'markdown','ext': '.md'}]
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

"{{{Tabular plugin tricks
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction
"}}}

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

" COC SETUP

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

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
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

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)
nnoremap <leader>cs :CocSearch <C-R>=expand("<cword>")<CR><CR>

" Formatting selected code.
xmap <leader>=  <Plug>(coc-format-selected)
nmap <leader>=  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
"xmap <leader>a  <Plug>(coc-codeaction-selected)
"nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)


" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

