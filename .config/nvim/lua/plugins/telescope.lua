return {
  "nvim-telescope/telescope.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  keys = {
    { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
    { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep" },
    { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
    { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
    { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
    { "<leader>fw", "<cmd>Telescope grep_string<cr>", desc = "Grep Word" },
    { "<leader>fs", function() require("telescope.builtin").lsp_document_symbols() end, desc = "Symbols" },
    { "<leader>fS", function() require("telescope.builtin").lsp_workspace_symbols() end, desc = "Workspace Symbols" },
    { "<leader>fgc", "<cmd>Telescope git_commits<cr>", desc = "Git Commits" },
    { "<leader>fgs", "<cmd>Telescope git_status<cr>", desc = "Git Status" },
    { "<leader>fgd", "<cmd>Telescope diagnostics<cr>", desc = "Diagnostics" },
  },
}
