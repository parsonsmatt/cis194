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
  fibs = [0,1]
  (1..x).each do |n|
    f1 = fibs[n-1]
    f2 = fibs[n-2] 
    fibs[n] = f1 + f2
  end
  fibs[x-1]
end
