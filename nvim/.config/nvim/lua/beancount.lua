local lspconfig = require 'lspconfig'
local completion = require 'completion'
lspconfig.beancount.setup= {
    on_attach=completion.on_attach,
    init_options = {
      pythonPath = "~/.local/share/virtualenvs/Beancount-AcabNhNa/bin/python3";
    },
};

--local lspconfig = require('lspconfig')
--local completion = require('completion')

--lspconfig.beancount.setup({
    --cmd = {
        --'node',
        --'--inspect',
        --'/home/brian/repos/beancount-language-server/out/cli.js',
        ----'beancount-langserver',
        --'--stdio'
    --},
    --on_attach=completion.on_attach,
    --init_options = {
      --pythonPath = "~/.local/share/virtualenvs/Beancount-AcabNhNa/bin/python3";
    --},
--})
