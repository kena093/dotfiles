return {
  "stevearc/oil.nvim",
  lazy = false,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require("oil").setup({
      view_options = {
        show_hidden = true,
        is_always_hidden = function(name)
          return vim.endswith(name, ".uid")
        end,
      },
      use_default_keymaps = false,
      keymaps = {
        ["<CR>"] = "actions.select",
        ["<leader>p"] = "actions.preview",
        ["<leader>r"] = "actions.refresh",
        ["<C-v>"] = { "actions.select", opts = { vertical = true }, desc = "Open in vertical split" },
        ["<C-s>"] = { "actions.select", opts = { horizontal = true }, desc = "Open in horizontal split" },
      },
    })

    -- Disable cmp in oil buffers
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "oil",
      callback = function()
        local ok, cmp = pcall(require, "cmp")
        if ok then cmp.setup.buffer({ enabled = false }) end
      end,
    })
  end,
}
