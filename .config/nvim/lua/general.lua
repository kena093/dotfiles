local opt = vim.opt

vim.g.mapleader = " "
opt.number = true
opt.relativenumber = true
vim.g.zig_fmt_autosave = 1
opt.autoread = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.smartindent = true
opt.undofile = true


-- key sequence wait time(ms)
opt.timeoutlen = 300
-- clipboard
if vim.fn.has("wsl") == 1 then
  vim.g.clipboard = {
    name = "WslClipboard",
    copy = {
      ["+"] = "clip.exe",
      ["*"] = "clip.exe",
    },
    paste = {
      ["+"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
      ["*"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    },
    cache_enabled = 0,
  }
end

-- これで y や d で自動的にシステムのクリップボードに入ります
vim.opt.clipboard = "unnamedplus"

if vim.g.neovide then
  vim.opt.guifont = "JetBrains_Mono_NL:h13"
  vim.g.neovide_scroll_animation_length = 0
  vim.opt.termguicolors = true

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.zig",
    callback = function()
      -- mise のフルパスを直接指定
      local mise_bin = "/usr/bin/mise"
      
      -- プロジェクトの zig パスを取得
      local zig_path = vim.fn.trim(vim.fn.system(mise_bin .. " which zig"))
      
      -- zig が見つからない（shell_error）なら何もしない
      if vim.v.shell_error ~= 0 or zig_path == "" then
        return
      end
  
      -- 現在のバッファの内容を stdin 経由で整形して書き換える
      local bufnr = vim.api.nvim_get_current_buf()
      local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
      local input = table.concat(lines, "\n")
      
      local output = vim.fn.system({ zig_path, "fmt", "--stdin" }, input)
  
      if vim.v.shell_error == 0 then
          local new_lines = vim.split(output, "\n")
          -- 末尾の空行を調整
          if new_lines[#new_lines] == "" then table.remove(new_lines) end
          vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, new_lines)
      end
    end,
  })
end

