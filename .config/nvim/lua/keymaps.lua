vim.keymap.set('n', '<leader>q', '<cmd>qa<CR>')
vim.keymap.set('n', '<leader>w', '<cmd>w<CR>')
vim.keymap.set('n', '<leader>n', '<cmd>nohl<CR>')

vim.keymap.set("n", "<leader>t", "<CMD>Oil<CR>", { desc = "Open file explorer" })
vim.keymap.set('n', "<leader>ss", "<CMD>luafile %<CR>", { desc = "Execute current lua" })
vim.keymap.set('n', '<leader>bo', '<cmd>silent! %bd|e#|bd#<cr>', { desc = "Close all buffers except current" })

vim.keymap.set('n', '>', '>>')
vim.keymap.set('n', '<', '<<')
vim.keymap.set('x', '>', '>gv')
vim.keymap.set('x', '<', '<gv')

vim.keymap.set('v', '<C-c>', function()
  vim.schedule(function()
    vim.cmd('normal! "+y')
  end)
end)

vim.keymap.set({ 'n', 'v' }, '<leader>v', function()
  vim.schedule(function()
    vim.cmd('normal! "+p')
  end)
end)

vim.keymap.set('n', '<Leader>cm', function()
  vim.fn.setreg('+', vim.fn.execute('messages'))
end)

vim.keymap.set('n', 'gh', function()
  vim.lsp.buf.hover()
end)

vim.keymap.set('v', '<leader>p', '"_dP')
vim.keymap.set('i', '<C-BS>', '<C-W>', { noremap = true })


if vim.g.neovide == true then
  pcall(function() vim.keymap.del("n", "<C-^>") end)
  vim.api.nvim_set_keymap("n", "<C-^>",
    ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1<CR>", { silent = true })
  vim.api.nvim_set_keymap("n", "<C-->",
    ":lua vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1<CR>", { silent = true })
end

local function map_nop(key, mode)
  vim.keymap.set(mode, key, '<Nop>', { noremap = true })
end

vim.keymap.set("n", "J", "<C-d>zz", { desc = "Scroll down and center" })

vim.keymap.set("n", "K", "<C-u>zz", { desc = "Scroll up and center" })

vim.keymap.set({ "n", "v" }, "H", "^", { desc = "Go to first non-blank character" })

vim.keymap.set({ "n", "v" }, "L", "$", { desc = "Go to end of line" })

vim.keymap.set("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })

vim.keymap.set("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next Buffer" })
