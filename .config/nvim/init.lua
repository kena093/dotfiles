-- 1. 前提設定の読み込み
require("preload")
require("general")
require("utils")
require("keymaps")

-- 2. プラグインマネージャ (lazy.nvim) の起動準備
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- 3. プラグイン設定
require("lazy").setup({
  -- === Navigation & Warp ===
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local harpoon = require("harpoon")
      harpoon:setup()

      vim.keymap.set("n", "<leader>a", function() harpoon:list():add() end)
      vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)
      vim.keymap.set("n", "<C-h>", function() harpoon:list():select(1) end)
      vim.keymap.set("n", "<C-t>", function() harpoon:list():select(2) end)
      vim.keymap.set("n", "<C-n>", function() harpoon:list():select(3) end)
      vim.keymap.set("n", "<C-s>", function() harpoon:list():select(4) end)
    end,
  },

  -- === Terminal (ToggleTerm) ===
  {
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
      local harpoon = require("harpoon")

      local function close_harpoon_menu()
        if harpoon.ui.win_id and vim.api.nvim_win_is_valid(harpoon.ui.win_id) then
          harpoon.ui:close_menu()
        end
      end

      local current_term = nil
      local function switch_terminal(target_term)
        return function()
          close_harpoon_menu()
          if current_term == target_term then
            vim.cmd("hide")
            current_term = nil
            return
          end
          if current_term ~= nil then vim.cmd("hide") end
          local dir = _G.Cwd and _G.Cwd() or vim.fn.getcwd()
          vim.cmd(string.format("%dToggleTerm dir=%s", target_term, dir))
          current_term = target_term
        end
      end

      vim.keymap.set("n", "<leader>1", switch_terminal(1))
      vim.keymap.set("n", "<leader>2", switch_terminal(2))
      vim.keymap.set("n", "<leader>3", switch_terminal(3))
      vim.keymap.set("n", "<leader>4", switch_terminal(4))

      function _G.set_terminal_keymaps()
        local k_opts = { buffer = 0 }
        vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], k_opts)
        vim.keymap.set("t", "<C-1>", [[<C-\><C-n><cmd>lua vim.api.nvim_input('<C-1>')<CR>]], k_opts)
        vim.keymap.set("t", "<C-2>", [[<C-\><C-n><cmd>lua vim.api.nvim_input('<C-2>')<CR>]], k_opts)
        vim.keymap.set("t", "<C-3>", [[<C-\><C-n><cmd>lua vim.api.nvim_input('<C-3>')<CR>]], k_opts)
      end

      opts.on_open = _G.set_terminal_keymaps
      toggleterm.setup(opts)
    end,
  },

  -- === Appearance & UI ===
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = function()
      require("gruvbox").setup({ contrast = "hard", italic = { comments = true } })
      vim.cmd("colorscheme gruvbox")
    end,
  },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "nvim-lualine/lualine.nvim", opts = {} },
  { "rcarriga/nvim-notify", opts = {} },
  { "mvllow/modes.nvim", event = "VeryLazy", opts = { line_opacity = 0.15 } },

  -- === Navigation & Search ===
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep" },
    },
  },

  -- === Coding & LSP ===
  {
    "neovim/nvim-lspconfig",
  },
{
    "hrsh7th/nvim-cmp",
    dependencies = { "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path" },
    config = function()
      local cmp = require("cmp")
      cmp.setup({
        sources = cmp.config.sources({
          { name = "nvim_lsp" }, 
          { name = "buffer" },
          { name = "path" },
        }),
        mapping = cmp.mapping.preset.insert({
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp.mapping.select_next_item(),
          ["S-<Tab>"] = cmp.mapping.select_prev_item(),
          ["C-n"] = cmp.mapping.select_next_item(),
          ["C-p"] = cmp.mapping.select_prev_item(),
        }),
      })
    end,
  },
  { 'ziglang/zig.vim' },
  { "windwp/nvim-autopairs", event = "InsertEnter", opts = {} },
  { "cordx56/rustowl", opts = { auto_enable = true } },
  {
    "stevearc/conform.nvim",
    opts = {
      format_on_save = { timeout_ms = 500, lsp_fallback = true },
    },
  },

  -- === Git ===
  { "lewis6991/gitsigns.nvim", opts = {} },
  {
    "sindrets/diffview.nvim",
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Git Diff" },
      { "<leader>gq", "<cmd>DiffviewClose<cr>", desc = "Close Diffview" },
    },
  },

  -- === Utilities & Tools ===
  { "folke/which-key.nvim", opts = { preset = "helix" } },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = { picker = { enabled = true }, lazygit = { enabled = true } },
    keys = {
      { "<leader><leader>", function() Snacks.picker.smart() end, desc = "Smart Picker" },
      { "<leader>gg", function() Snacks.lazygit.open() end, desc = "LazyGit" },
    },
  },
  { "WilliamHsieh/overlook.nvim" },
  { "folke/trouble.nvim", opts = {} },
  {
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
        },
      })

      local cmp = require("cmp")
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "oil",
        callback = function()
          cmp.setup.buffer({ enabled = false })
        end,
      })
    end,
  },
})

-- 4. LSP 設定のロード
require("lsp")
