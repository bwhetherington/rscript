foo = 10


def test():
    # print(foo)
    foo = 20
    print(foo)
    if True:
        foo = 30
        print(foo)
    print(foo)


if __name__ == "__main__":
    test()
