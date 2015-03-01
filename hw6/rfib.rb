def fib x
  0 if x == 0
  1 if x == 1
  fib(x-1) + fib(x-2)
end

def fib2 x
  fibs = [0,1]
  (1..x).each do |n|
    fibs[n] = fibs[n-1] + fibs[n-2] 
  end
  fibs[x-1]
end
