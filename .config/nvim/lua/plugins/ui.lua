return {
  { "nvim-tree/nvim-web-devicons", lazy = true },

  { "folke/which-key.nvim", opts = { preset = "helix" } },

  {
    "echasnovski/mini.indentscope",
    version = false,
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      symbol = "|",
      options = { try_as_border = true },
      draw = {
        delay = 0,
        animation = function() return 0 end,
      },
    },
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "help", "alpha", "dashboard", "neo-tree",
          "Trouble", "lazy", "mason", "notify", "toggleterm",
        },
        callback = function()
          vim.b.miniindentscope_disable = true
        end,
      })
    end,
  },

  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    opts = { win = { size = 0.3 } },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle focus=true<cr>", desc = "Diagnostics" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics" },
      { "<leader>xs", "<cmd>Trouble symbols toggle focus=true<cr>", desc = "Symbols" },
      { "gr", "<cmd>Trouble lsp_references toggle focus=true<cr>", desc = "References" },
      { "gi", "<cmd>Trouble lsp_implementations toggle focus=true<cr>", desc = "Implementations" },
      { "<leader>xl", "<cmd>Trouble lsp toggle focus=false win.position=right<cr>", desc = "LSP Definitions" },
      { "<leader>xL", "<cmd>Trouble loclist toggle<cr>", desc = "Location List" },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix List" },
    },
  },

  {
    "nacro90/numb.nvim",
    config = function() require("numb").setup() end,
  },
}
