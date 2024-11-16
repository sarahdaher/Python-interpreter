l = [2+i for i in [1,2,3]]
print(l)

def f(x):
   return x+1

print([f(i) for i in range(4)])
print([i for i in range(4) if i % 2 == 0])
print([f(i) for i in range(4) if i % 2 == 0])
print([f(i) for i in l if f(i) % 2 != 0])
print([i for i in range(1, 10)])
print([i for i in range(2, 10, 2)])
print([i for i in range(2, 11, 2)])

l = [1, 2, 3, 4]
l = [i * i for i in l]
print(l)
