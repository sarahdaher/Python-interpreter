def f(x):
    if x <= 1:
        return 1
    return f(x - 1) + f(x - 2)

print(f(10))
print(f(30))
