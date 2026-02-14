-- 1. カレントディレクトリ取得 (oil.nvim との連携)
-- ターミナルやファイラーを開く際の基準パスを返します
function Cwd()
  local ok, oil = pcall(require, "oil")
  local oil_dir = ok and oil.get_current_dir()
  if oil_dir then return oil_dir end

  local path = vim.fn.expand('%:p:h')
  -- ターミナルバッファの場合はパスを返さない
  if vim.api.nvim_buf_get_name(0):match("^term://") then return nil end
  return path
end

-- 2. プロジェクトのルートディレクトリ取得
-- Git管理されている場合は .git がある場所を、そうでなければ現在のディレクトリを返します
function GetRootDir()
  local path = Cwd() or vim.fn.expand('%:p:h')
  local git_root = vim.fn.systemlist('git -C ' .. vim.fn.shellescape(path) .. ' rev-parse --show-toplevel')[1]
  if vim.v.shell_error == 0 and git_root then
    return git_root
  end
  return path
end

-- 3. OS判定 (WSL専用のため固定)
function IsWindows() return false end
function IsLinux() return true end