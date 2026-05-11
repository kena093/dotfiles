-- Acme editor color scheme for Neovim
-- Inspired by Plan 9's Acme editor

vim.cmd("hi clear")
if vim.fn.exists("syntax_on") then
	vim.cmd("syntax reset")
end

vim.o.background = "light"
vim.g.colors_name = "acme"

local colors = {
	bg = "#ffffea",        -- Acme yellow background
	fg = "#000000",        -- Black text
	tag_bg = "#eaffff",    -- Acme blue tag bar
	tag_fg = "#000000",    -- Black text on tag
	scroll_bg = "#99994c", -- Scrollbar
	border = "#888888",    -- Border color
	comment = "#006600",   -- Green comments
	string = "#990000",    -- Red strings
	keyword = "#000099",   -- Blue keywords
	func = "#000000",      -- Black functions
	type = "#006600",      -- Green types
	number = "#990000",    -- Red numbers
	error = "#cc0000",     -- Error red
	warning = "#999900",   -- Warning yellow
	selection = "#eaffff", -- Selection (same as tag)
	search = "#ccffcc",    -- Search highlight
	visual = "#eaffff",    -- Visual selection
	cursor_line = "#ffffcc", -- Subtle cursor line
	line_nr = "#888888",   -- Line numbers
	match_paren = "#ccccff", -- Matching parenthesis
}

local hi = function(group, opts)
	vim.api.nvim_set_hl(0, group, opts)
end

-- Editor
hi("Normal", { fg = colors.fg, bg = colors.bg })
hi("NormalFloat", { fg = colors.fg, bg = colors.tag_bg })
hi("FloatBorder", { fg = colors.border, bg = colors.tag_bg })
hi("Cursor", { fg = colors.bg, bg = colors.fg })
hi("CursorLine", { bg = colors.cursor_line })
hi("CursorColumn", { bg = colors.cursor_line })
hi("LineNr", { fg = colors.line_nr, bg = colors.bg })
hi("CursorLineNr", { fg = colors.fg, bg = colors.cursor_line, bold = true })
hi("SignColumn", { fg = colors.fg, bg = colors.bg })
hi("VertSplit", { fg = colors.border, bg = colors.bg })
hi("WinSeparator", { fg = colors.border, bg = colors.bg })
hi("ColorColumn", { bg = colors.cursor_line })
hi("Folded", { fg = colors.comment, bg = colors.tag_bg })
hi("FoldColumn", { fg = colors.line_nr, bg = colors.bg })
hi("MatchParen", { bg = colors.match_paren, bold = true })
hi("NonText", { fg = colors.line_nr })
hi("SpecialKey", { fg = colors.line_nr })
hi("Whitespace", { fg = colors.line_nr })
hi("EndOfBuffer", { fg = colors.bg })

-- Search
hi("Search", { fg = colors.fg, bg = colors.search })
hi("IncSearch", { fg = colors.bg, bg = colors.fg })
hi("CurSearch", { fg = colors.bg, bg = colors.fg })
hi("Substitute", { fg = colors.bg, bg = colors.error })

-- Selection
hi("Visual", { bg = colors.visual })
hi("VisualNOS", { bg = colors.visual })

-- Statusline (tag bar style)
hi("StatusLine", { fg = colors.tag_fg, bg = colors.tag_bg })
hi("StatusLineNC", { fg = colors.line_nr, bg = colors.tag_bg })
hi("WildMenu", { fg = colors.fg, bg = colors.selection })

-- Tabline
hi("TabLine", { fg = colors.line_nr, bg = colors.tag_bg })
hi("TabLineFill", { bg = colors.tag_bg })
hi("TabLineSel", { fg = colors.fg, bg = colors.bg, bold = true })

-- Pmenu (completion menu)
hi("Pmenu", { fg = colors.fg, bg = colors.tag_bg })
hi("PmenuSel", { fg = colors.fg, bg = colors.selection })
hi("PmenuSbar", { bg = colors.tag_bg })
hi("PmenuThumb", { bg = colors.scroll_bg })

-- Messages
hi("ErrorMsg", { fg = colors.error, bg = colors.bg })
hi("WarningMsg", { fg = colors.warning, bg = colors.bg })
hi("ModeMsg", { fg = colors.fg, bg = colors.bg, bold = true })
hi("MoreMsg", { fg = colors.comment, bg = colors.bg })
hi("Question", { fg = colors.comment, bg = colors.bg })
hi("Title", { fg = colors.fg, bold = true })

-- Diff
hi("DiffAdd", { bg = "#ccffcc" })
hi("DiffChange", { bg = "#ffffcc" })
hi("DiffDelete", { fg = colors.error, bg = "#ffcccc" })
hi("DiffText", { bg = "#ccccff" })

-- Spelling
hi("SpellBad", { undercurl = true, sp = colors.error })
hi("SpellCap", { undercurl = true, sp = colors.warning })
hi("SpellLocal", { undercurl = true, sp = colors.comment })
hi("SpellRare", { undercurl = true, sp = colors.keyword })

-- Syntax
hi("Comment", { fg = colors.comment, italic = true })
hi("Constant", { fg = colors.string })
hi("String", { fg = colors.string })
hi("Character", { fg = colors.string })
hi("Number", { fg = colors.number })
hi("Boolean", { fg = colors.keyword })
hi("Float", { fg = colors.number })
hi("Identifier", { fg = colors.fg })
hi("Function", { fg = colors.func })
hi("Statement", { fg = colors.keyword, bold = true })
hi("Conditional", { fg = colors.keyword, bold = true })
hi("Repeat", { fg = colors.keyword, bold = true })
hi("Label", { fg = colors.keyword })
hi("Operator", { fg = colors.fg })
hi("Keyword", { fg = colors.keyword, bold = true })
hi("Exception", { fg = colors.keyword, bold = true })
hi("PreProc", { fg = colors.keyword })
hi("Include", { fg = colors.keyword })
hi("Define", { fg = colors.keyword })
hi("Macro", { fg = colors.keyword })
hi("PreCondit", { fg = colors.keyword })
hi("Type", { fg = colors.type })
hi("StorageClass", { fg = colors.keyword })
hi("Structure", { fg = colors.type })
hi("Typedef", { fg = colors.type })
hi("Special", { fg = colors.string })
hi("SpecialChar", { fg = colors.string })
hi("Tag", { fg = colors.keyword })
hi("Delimiter", { fg = colors.fg })
hi("SpecialComment", { fg = colors.comment, bold = true })
hi("Debug", { fg = colors.error })
hi("Underlined", { fg = colors.keyword, underline = true })
hi("Ignore", { fg = colors.line_nr })
hi("Error", { fg = colors.error, bold = true })
hi("Todo", { fg = colors.fg, bg = colors.tag_bg, bold = true })

-- Treesitter
hi("@comment", { link = "Comment" })
hi("@string", { link = "String" })
hi("@number", { link = "Number" })
hi("@boolean", { link = "Boolean" })
hi("@function", { link = "Function" })
hi("@function.builtin", { fg = colors.func })
hi("@keyword", { link = "Keyword" })
hi("@type", { link = "Type" })
hi("@type.builtin", { fg = colors.type })
hi("@variable", { fg = colors.fg })
hi("@variable.builtin", { fg = colors.fg, italic = true })
hi("@constant", { fg = colors.fg })
hi("@constant.builtin", { fg = colors.keyword })
hi("@property", { fg = colors.fg })
hi("@field", { fg = colors.fg })
hi("@parameter", { fg = colors.fg })
hi("@punctuation", { fg = colors.fg })
hi("@punctuation.bracket", { fg = colors.fg })
hi("@punctuation.delimiter", { fg = colors.fg })
hi("@operator", { fg = colors.fg })
hi("@tag", { fg = colors.keyword })
hi("@tag.attribute", { fg = colors.fg })
hi("@tag.delimiter", { fg = colors.fg })

-- LSP
hi("DiagnosticError", { fg = colors.error })
hi("DiagnosticWarn", { fg = colors.warning })
hi("DiagnosticInfo", { fg = colors.keyword })
hi("DiagnosticHint", { fg = colors.comment })
hi("DiagnosticUnderlineError", { undercurl = true, sp = colors.error })
hi("DiagnosticUnderlineWarn", { undercurl = true, sp = colors.warning })
hi("DiagnosticUnderlineInfo", { undercurl = true, sp = colors.keyword })
hi("DiagnosticUnderlineHint", { undercurl = true, sp = colors.comment })
hi("LspReferenceText", { bg = colors.selection })
hi("LspReferenceRead", { bg = colors.selection })
hi("LspReferenceWrite", { bg = colors.selection })
hi("LspInlayHint", { fg = colors.line_nr, italic = true })

-- nvim-tree
hi("NvimTreeNormal", { fg = colors.fg, bg = colors.bg })
hi("NvimTreeFolderName", { fg = colors.fg })
hi("NvimTreeFolderIcon", { fg = colors.keyword })
hi("NvimTreeOpenedFolderName", { fg = colors.fg, bold = true })
hi("NvimTreeRootFolder", { fg = colors.comment })
hi("NvimTreeGitDirty", { fg = colors.warning })
hi("NvimTreeGitNew", { fg = colors.comment })

-- Telescope
hi("TelescopeNormal", { fg = colors.fg, bg = colors.bg })
hi("TelescopeBorder", { fg = colors.border, bg = colors.bg })
hi("TelescopePromptBorder", { fg = colors.border, bg = colors.bg })
hi("TelescopeResultsBorder", { fg = colors.border, bg = colors.bg })
hi("TelescopePreviewBorder", { fg = colors.border, bg = colors.bg })
hi("TelescopeSelection", { bg = colors.selection })
hi("TelescopeMatching", { fg = colors.keyword, bold = true })

-- Git signs
hi("GitSignsAdd", { fg = colors.comment })
hi("GitSignsChange", { fg = colors.warning })
hi("GitSignsDelete", { fg = colors.error })
