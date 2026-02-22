-- 1. 前提設定の読み込み
require("preload")
require("general")
require("utils")
require("keymaps")

if vim.fn.has("win32") == 1 then
  vim.opt.shell = "bash"
  vim.opt.shellcmdflag = "-c"
  vim.opt.shellquote = '"'
  vim.opt.shellxquote = "" 
end

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
      local function safe_hide()
        if #vim.api.nvim_list_wins() > 1 then
          vim.cmd("hide")
        else
          vim.cmd("bp") 
        end
      end
      local function switch_terminal(target_term)
        return function()
          close_harpoon_menu()
          if current_term == target_term then
            safe_hide()
            current_term = nil
            return
          end
          if current_term ~= nil then safe_hide() end
          local dir = _G.Cwd and _G.Cwd() or vim.fn.getcwd()
          vim.cmd(string.format("%dToggleTerm dir=%s", target_term, dir))
          current_term = target_term
        end
      end
      for i = 1, 4 do
        vim.keymap.set("n", "<leader>" .. i, switch_terminal(i))
      end
      function _G.set_terminal_keymaps()
        local k_opts = { buffer = 0 }
        vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], k_opts)
        for i = 1, 4 do
          vim.keymap.set("t", "<C-"..i..">", [[<C-\><C-n><cmd>lua vim.api.nvim_input('<leader>]]..i..[[')<CR>]], k_opts)
        end
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

      -- 検索
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep" },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
      { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
    
      -- Git
      { "<leader>fc", "<cmd>Telescope git_commits<cr>", desc = "Git Commits" },
      { "<leader>fs", "<cmd>Telescope git_status<cr>", desc = "Git Status" },
      { "<leader>fd", "<cmd>Telescope diagnostics<cr>", desc = "Diagnostics" },
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
    opts = { picker = { enabled = true, lazygit = { enabled = true } } },
    keys = {
      -- スマートピッカー
      { "<leader><leader>", function() Snacks.picker.smart() end, desc = "Smart Picker" },
      { "<leader>gg", function() Snacks.lazygit.open() end, desc = "LazyGit" },

      -- 検索系
      { "<leader>sf", function() Snacks.picker.files() end, desc = "Find Files" },
      { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep (Text Search)" },
      { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word" },
      { "<leader>sb", function() Snacks.picker.buffers() end, desc = "Buffers" },
      { "<leader>sr", function() Snacks.picker.recent() end, desc = "Recent Files" },

      -- システム 
      { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
      { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
      { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
      { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
      { "<leader>bd", function() Snacks.bufdelete() end, desc = "Close Buffer" },
    
      -- LSP
      { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
      { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume Last Picker" },
    },
  },
  {
    "WilliamHsieh/overlook.nvim",
    config = function()
      require("overlook").setup({})
    end,
    keys = {
      { "<leader>pd", function() require("overlook.api").peek_definition() end, desc = "Peek definition" },
      { "<leader>pp", function() require("overlook.api").peek_cursor() end, desc = "Peek cursor" },
      { "<leader>pc", function() require("overlook.api").close_all() end, desc = "Close all popups" },
      { "<leader>pu", function() require("overlook.api").restore_popup() end, desc = "Restore last popup" },
      { "<leader>pU", function() require("overlook.api").restore_all_popups() end, desc = "Restore all popups" },
      { "<leader>pf", function() require("overlook.api").switch_focus() end, desc = "Switch focus" },
      { "<leader>ps", function() require("overlook.api").open_in_split() end, desc = "Open popup in split" },
      { "<leader>pv", function() require("overlook.api").open_in_vsplit() end, desc = "Open popup in vsplit" },
      { "<leader>pt", function() require("overlook.api").open_in_tab() end, desc = "Open popup in tab" },
      { "<leader>po", function() require("overlook.api").open_in_original_window() end, desc = "Open popup in current window" },
    },
  },
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
          ["<C-v>"] = { "actions.select", opts = { vertical = true }, desc = "Open the selection in a vertical split" },
          ["<C-s>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the selection in a horizontal split" },
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
