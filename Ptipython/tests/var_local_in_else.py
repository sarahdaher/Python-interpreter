def f():
    if False:
        y = 42
    else:
        y = 12
    print(y)

    if False:
        print("KO")
    else:
        print("OK")
        z = 9
    print(z)
    print(y)

f()
