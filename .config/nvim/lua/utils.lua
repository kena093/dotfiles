--- Current directory (integrates with oil.nvim)
function Cwd()
  local ok, oil = pcall(require, "oil")
  local oil_dir = ok and oil.get_current_dir()
  if oil_dir then return oil_dir end

  if vim.api.nvim_buf_get_name(0):match("^term://") then return nil end
  return vim.fn.expand("%:p:h")
end

--- Project root directory (git root, or cwd as fallback)
function GetRootDir()
  local path = Cwd() or vim.fn.expand("%:p:h")
  local git_root = vim.fn.systemlist("git -C " .. vim.fn.shellescape(path) .. " rev-parse --show-toplevel")[1]
  if vim.v.shell_error == 0 and git_root then return git_root end
  return path
end

function IsWindows() return false end
function IsLinux() return true end
