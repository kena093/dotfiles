local opt = vim.opt

-- General
opt.number = true
opt.autoread = true
opt.undofile = true
opt.swapfile = false
opt.showmode = false
opt.termguicolors = true
opt.signcolumn = "yes"
opt.cursorline = true
opt.laststatus = 2
opt.scrolloff = 7
opt.timeoutlen = 300

-- Indentation
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

-- Appearance
opt.fillchars = { vert = "|", horiz = "-", eob = " " }

-- Clipboard (WSL)
if vim.fn.has("wsl") == 1 then
  vim.g.clipboard = {
    name = "WslClipboard",
    copy = { ["+"] = "clip.exe", ["*"] = "clip.exe" },
    paste = {
      ["+"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
      ["*"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    },
    cache_enabled = 0,
  }
end
opt.clipboard = "unnamedplus"

-- Zig
vim.g.zig_fmt_autosave = 1

-- Colorscheme
vim.cmd("colorscheme acme")
