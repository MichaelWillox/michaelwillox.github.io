-- {{< publications >}} shortcode
--
-- Lists the publications in data/publications.yml (loaded into the page's
-- metadata with `metadata-files`). Works for HTML and Typst/PDF, no R needed.
--
-- Options:
--   level=2          heading level for the section titles
--   abstracts=true   add a collapsible abstract under each entry (HTML only)
--   type=journal     show only one section (journal | report | working | phd)
--   me=Willox        family name to show in bold

local sections = {
  { key = "journal", title = "Peer-Reviewed Journal Articles" },
  { key = "report",  title = "Working Papers and Reports, Statistics Canada" },
  { key = "working", title = "Work in Progress" },
  { key = "phd",     title = "PhD Thesis Papers, Brunel University London" },
}

local str = pandoc.utils.stringify

local function has(x)
  return x ~= nil and str(x) ~= ""
end

local function truthy(x)
  if x == nil then return false end
  if type(x) == "boolean" then return x end
  local s = str(x):lower()
  return s == "true" or s == "yes"
end

-- "Gu, Wulong" -> "Gu, W."; "Baldwin, John R." -> "Baldwin, J. R."
local function short_name(name)
  local family, given = name:match("^%s*(.-)%s*,%s*(.-)%s*$")
  if not family then return name end
  local initials = {}
  for g in given:gmatch("%S+") do
    if g:sub(-1) == "." then
      table.insert(initials, g)
    else
      table.insert(initials, pandoc.text.sub(g, 1, 1) .. ".")
    end
  end
  return family .. ", " .. table.concat(initials, " ")
end

local function author_inlines(authors, me)
  local names = {}
  for _, a in ipairs(authors or {}) do
    table.insert(names, short_name(str(a)))
  end
  local out = pandoc.Inlines({})
  local n = #names
  for i, nm in ipairs(names) do
    if i > 1 then
      out:insert(pandoc.Str(n > 1 and i == n and ", & " or ", "))
    end
    local inl = pandoc.Inlines(pandoc.Str(nm))
    if nm:sub(1, #me) == me then inl = pandoc.Inlines(pandoc.Strong(inl)) end
    out:extend(inl)
  end
  return out
end

local function as_inlines(x)
  if pandoc.utils.type(x) == "Inlines" then return x:clone() end
  if pandoc.utils.type(x) == "Blocks" then
    return pandoc.utils.blocks_to_inlines(x)
  end
  return pandoc.Inlines(pandoc.Str(str(x)))
end

local function text(s)
  return pandoc.Inlines(pandoc.read(s, "markdown").blocks[1].content)
end

local function citation(p, me)
  local c = author_inlines(p.authors, me)
  c:insert(pandoc.Space())
  c:insert(pandoc.Str("(" .. str(p.year) .. ")."))
  c:insert(pandoc.Space())
  local title = as_inlines(p.title)
  if has(p.url) then
    c:insert(pandoc.Link(title, str(p.url)))
  else
    c:extend(title)
  end
  c:insert(pandoc.Str("."))
  if has(p.venue) then
    c:insert(pandoc.Space())
    c:insert(pandoc.Emph(as_inlines(p.venue)))
    local tail = ""
    if has(p.volume) then tail = tail .. ", " .. str(p.volume) end
    if has(p.issue)  then tail = tail .. "(" .. str(p.issue) .. ")" end
    if has(p.pages)  then tail = tail .. ", " .. str(p.pages) end
    if has(p.number) then tail = tail .. ", " .. str(p.number) end
    c:insert(pandoc.Str(tail .. "."))
  end
  if has(p.publisher) then
    c:insert(pandoc.Space())
    c:extend(as_inlines(p.publisher))
    c:insert(pandoc.Str("."))
  end
  if has(p.ungated) then
    c:insert(pandoc.Space())
    c:insert(pandoc.Link(text("Ungated version"), str(p.ungated)))
    c:insert(pandoc.Str("."))
  end
  if has(p.note) then
    c:insert(pandoc.Space())
    c:extend(as_inlines(p.note))
  end
  return c
end

local function year_key(p)
  return tonumber(str(p.year or "")) or math.huge -- "in progress" first
end

return {
  ["publications"] = function(args, kwargs, meta)
    local pubs = meta.publications
    if pubs == nil then
      quarto.log.warning("publications shortcode: no `publications` metadata. "
        .. "Add `metadata-files: [../data/publications.yml]` to the page.")
      return pandoc.Blocks({})
    end
    local level = tonumber(str(kwargs.level or "")) or 2
    local abstracts = truthy(kwargs.abstracts) and quarto.doc.is_format("html")
    local only = has(kwargs.type) and str(kwargs.type) or nil
    local me = has(kwargs.me) and str(kwargs.me) or "Willox"

    local blocks = pandoc.Blocks({})
    for _, sec in ipairs(sections) do
      if only == nil or only == sec.key then
        local sel = {}
        for i, p in ipairs(pubs) do
          if str(p.type or "") == sec.key and not truthy(p.draft) then
            table.insert(sel, { p = p, i = i })
          end
        end
        table.sort(sel, function(a, b)
          local ya, yb = year_key(a.p), year_key(b.p)
          if ya ~= yb then return ya > yb end
          return a.i < b.i
        end)
        if #sel > 0 then
          if only == nil then
            blocks:insert(pandoc.Header(level, sec.title))
          end
          for _, e in ipairs(sel) do
            blocks:insert(pandoc.Para(citation(e.p, me)))
            if abstracts and has(e.p.abstract) then
              local abs = e.p.abstract
              local body = pandoc.utils.type(abs) == "Blocks" and abs
                or pandoc.Blocks({ pandoc.Para(as_inlines(abs)) })
              blocks:insert(pandoc.RawBlock("html",
                '<details class="pub-abstract"><summary>Abstract</summary>'))
              blocks:extend(body)
              blocks:insert(pandoc.RawBlock("html", "</details>"))
            end
          end
        end
      end
    end
    return blocks
  end
}
