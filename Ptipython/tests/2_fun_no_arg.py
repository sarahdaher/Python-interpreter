def g():
    print("OK g")

def f():
    print("OK f 1")
    g()
    print("OK f 2")

f()
print("OK global 1")
g()
f()
print("OK global 2")
