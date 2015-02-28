def fib x
  if x == 0
    0
  elsif x == 1
    1
  else
    fib(x-1) + fib(x-2)
  end
end

def fib2 x
  fibs = [1]
  for n in 1..x
    f1 = fibs[n-1] || fib(n-1)
    f2 = fibs[n-2] || fib(n-2)
    fibs[n] = f1 + f2
  end
  fibs[x-1]
end
