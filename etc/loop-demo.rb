def foo(n)
  i = 0
  while true do
    if i == 5000000 then
      return i
    else
      i += 1
    end
  end
end

puts(foo(0))
