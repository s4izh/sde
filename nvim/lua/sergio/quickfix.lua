function ToggleQuickfix()
  local qf_exists = false
  for _, win in pairs(vim.fn.getwininfo()) do
    if win.quickfix == 1 then
      qf_exists = true
      break
    end
  end

  if qf_exists then
    vim.cmd('cclose')
  else
    vim.cmd('copen')
  end
end

vim.api.nvim_set_keymap('n', '<Leader>q', ':lua ToggleQuickfix()<CR>', { noremap = true, silent = true })

vim.cmd[[
function! s:FilterQuickfixListByFile(bang, pattern)
  let cmp = a:bang ? '!~#' : '=~#'
  call setqflist(filter(getqflist(), "bufname(v:val['bufnr']) " . cmp . " a:pattern"))
endfunction
command! -bang -nargs=1 -complete=file QFilterFile call s:FilterQuickfixListByFile(<bang>0, <q-args>)
]]

vim.cmd[[
function! s:FilterQuickfixListByText(bang, pattern)
  " Check if quickfix list is empty or not set
  if empty(getqflist())
    echo "Quickfix list is empty."
    return
  endif

  " Use !~# (does NOT match) if bang is present, otherwise use =~# (matches)
  let comparison_operator = a:bang ? '!~#' : '=~#'

  " Construct the filter expression string for the filter() function.
  " v:val is the dictionary for each quickfix item.
  " We check the 'text' key against the provided pattern using the chosen operator.
  " We use single quotes around the pattern within the expression string.
  let filter_expression = 'v:val["text"] ' . comparison_operator . ' ''' . a:pattern . ''''

  try
    " Get a copy of the list to filter (avoids potential issues if filtering fails midway)
    let current_list = getqflist()
    let filtered_list = filter(copy(current_list), filter_expression)

    " Replace the current quickfix list with the filtered results
    call setqflist(filtered_list, 'r')

    " Report results
    let result_count = len(filtered_list)
    if result_count == 0
      echo "Quickfix filter removed all items."
    else
      echo "Quickfix filtered. " . result_count . " items remaining."
      " Optional: Automatically open/refresh the quickfix window
      cwindow
    endif
  catch /.*/
    echoerr "Error filtering quickfix list: " . v:exception
  endtry
endfunction

" -bang: enables the ! for negation
" -nargs=1: requires exactly one argument (the pattern)
command! -bang -nargs=1 QFilter call s:FilterQuickfixListByText(<bang>0, <q-args>)
]]
