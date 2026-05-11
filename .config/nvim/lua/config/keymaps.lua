local map = vim.keymap.set

-- Clear search highlight
map("n", "<leader>n", "<cmd>nohl<CR>")

-- Windows
map("n", "<leader>w=", "<C-w>=", { desc = "Normalize window sizes" })
map("n", "<leader>wm", "<cmd>only<cr>", { desc = "Maximize window" })

-- Buffers
map("n", "<leader>bo", "<cmd>silent! %bd|e#|bd#<cr>", { desc = "Close all buffers except current" })

-- File explorer (Oil)
map("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open file explorer" })

-- Indent (single key in normal mode)
map("n", ">", ">>")
map("n", "<", "<<")
map("x", ">", ">gv")
map("x", "<", "<gv")

-- Clipboard
map("v", "<C-c>", function()
  vim.schedule(function() vim.cmd('normal! "+y') end)
end)
map({ "n", "v" }, "<leader>v", function()
  vim.schedule(function() vim.cmd('normal! "+p') end)
end)
map("n", "<Leader>cm", function()
  vim.fn.setreg("+", vim.fn.execute("messages"))
end)

-- Paste over selection without yanking
map("v", "<leader>p", '"_dP')

-- Scroll and center
map("n", "<C-d>", "<C-d>zz", { desc = "Scroll down and center" })
map("n", "<C-u>", "<C-u>zz", { desc = "Scroll up and center" })

-- Resize with arrows
map("n", "<C-Up>", ":resize +1<CR>", { silent = true })
map("n", "<C-Down>", ":resize -1<CR>", { silent = true })
map("n", "<C-Left>", ":vertical resize -1<CR>", { silent = true })
map("n", "<C-Right>", ":vertical resize +1<CR>", { silent = true })

-- Disable Q (ex mode)
map("n", "Q", "<nop>")

-- Move current line
map("n", "<M-j>", "<cmd>m .+1<cr>", { desc = "Move Down" })
map("n", "<M-k>", "<cmd>m .-2<cr>", { desc = "Move Up" })

-- Terminal escape
map("t", "<Esc>", [[<C-\><C-n>]], { noremap = true })

-- Preserve default f/F/t/T (prevent plugin overrides)
map({ "n", "x", "o" }, "f", "f")
map({ "n", "x", "o" }, "F", "F")
map({ "n", "x", "o" }, "t", "t")
map({ "n", "x", "o" }, "T", "T")
