vim.lsp.enable("pyright")
vim.lsp.config("rust_analyzer", {
	settings = {
		["rust-analyzer"] = {
			rustc = {
				source = "discover",
			},
			cargo = {
				features = "all",
			},
		},
	},
})
vim.lsp.enable("rust_analyzer")
vim.lsp.enable("clangd")
vim.lsp.enable("zls")
on_lsp_attach(function(ev)
	vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
end)
