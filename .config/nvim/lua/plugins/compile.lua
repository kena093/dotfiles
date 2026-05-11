return {
  "ej-shafran/compile-mode.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "m00qek/baleia.nvim", tag = "v1.3.0" },
  },
  opts = { default_command = "make" },
  keys = {
    { "<leader>cc", "<cmd>Compile<cr>", desc = "Compile" },
    { "<leader>cr", "<cmd>Recompile<cr>", desc = "Recompile" },
  },
}
