return {
  "akinsho/toggleterm.nvim",
  version = "*",
  event = "VeryLazy",
  opts = {
    direction = "float",
    close_on_exit = true,
    float_opts = {
      border = "curved",
      winblend = 0,
      highlights = { border = "Normal", background = "Normal" },
    },
  },
  config = function(_, opts)
    local toggleterm = require("toggleterm")
    local current_term = nil

    local function safe_hide()
      if #vim.api.nvim_list_wins() > 1 then
        vim.cmd("hide")
      else
        vim.cmd("bp")
      end
    end

    local function switch_terminal(target)
      return function()
        if current_term == target then
          safe_hide()
          current_term = nil
          return
        end
        if current_term ~= nil then safe_hide() end
        local dir = _G.Cwd and _G.Cwd() or vim.fn.getcwd()
        vim.cmd(string.format("%dToggleTerm dir=%s", target, dir))
        current_term = target
      end
    end

    for i = 1, 4 do
      vim.keymap.set("n", "<leader>" .. i, switch_terminal(i))
    end

    function _G.set_terminal_keymaps()
      local k = { buffer = 0 }
      vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], k)
      for i = 1, 4 do
        vim.keymap.set("t", "<C-" .. i .. ">",
          [[<C-\><C-n><cmd>lua vim.api.nvim_input('<leader>]] .. i .. [[')<CR>]], k)
      end
    end

    opts.on_open = _G.set_terminal_keymaps
    toggleterm.setup(opts)
  end,
}
