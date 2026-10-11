-- Minimal `[[denote:ID][Title]]` hyperlink source for nvim-orgmode, resolving
-- IDs against ~/notes/org/. Small enough to own directly instead of pulling
-- in denote.nvim just for this one feature.

local ORG_DIR = vim.fn.expand("~/notes/org/")

local OrgLinkDenote = {}
OrgLinkDenote.__index = OrgLinkDenote

function OrgLinkDenote:new()
  return setmetatable({}, OrgLinkDenote)
end

function OrgLinkDenote:get_name()
  return "denote"
end

function OrgLinkDenote:_find_file(id)
  local matches = vim.fn.glob(ORG_DIR .. id .. "*", false, true)
  if #matches == 0 then
    return nil
  end
  if #matches > 1 then
    vim.notify("denote: more than one file with identifier " .. id, vim.log.levels.WARN)
  end
  return matches[1]
end

function OrgLinkDenote:follow(link)
  local id = link:match("^denote:(.+)$")
  if not id then
    return false
  end
  local file = self:_find_file(id)
  if not file then
    vim.notify("denote: no file with identifier " .. id, vim.log.levels.WARN)
    return false
  end
  vim.cmd("edit " .. vim.fn.fnameescape(file))
  return true
end

function OrgLinkDenote:autocomplete(context)
  local items = {}
  for _, path in ipairs(vim.fn.glob(ORG_DIR .. "*", false, true)) do
    local id = vim.fs.basename(path):match("^(%d%d%d%d%d%d%d%dT%d%d%d%d%d%d)")
    if id then
      local completion = "denote:" .. id
      if context.matcher(completion, context.base) then
        table.insert(items, completion)
      end
    end
  end
  return items
end

return OrgLinkDenote
