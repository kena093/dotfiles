-- Center cursor on entering insert mode
vim.api.nvim_create_autocmd("InsertEnter", {
  callback = function() vim.cmd("normal! zz") end,
})

-- C/H files use 2-space indent
vim.api.nvim_create_autocmd({ "BufRead", "BufEnter" }, {
  pattern = { "*.c", "*.h" },
  command = "setlocal tabstop=2 shiftwidth=2 expandtab",
})
