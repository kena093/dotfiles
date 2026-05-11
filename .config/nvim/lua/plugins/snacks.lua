return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = true },
    dashboard = { enabled = true },
    input = { enabled = true },
    notifier = { enabled = true },
    picker = { enabled = true },
    lazygit = { enabled = true },
  },
  keys = {
    { "<leader><leader>", function() Snacks.picker.smart() end, desc = "Smart Picker" },
    { "<leader>gg", function() Snacks.lazygit.open() end, desc = "LazyGit" },

    -- Search
    { "<leader>sf", function() Snacks.picker.files() end, desc = "Find Files" },
    { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Grep Word" },
    { "<leader>sb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>sr", function() Snacks.picker.recent() end, desc = "Recent Files" },

    -- System
    { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<leader>bd", function() Snacks.bufdelete() end, desc = "Close Buffer" },

    -- LSP
    { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume Last Picker" },
    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<leader>su", function() Snacks.picker.undo() end, desc = "Undo History" },
    { "<leader>sr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
  },
}
