class Test:
    def __init__(self, value):
        self.value = value

    def test(self):
        return self.value

foo = Test(10)
print(foo.test())

test = foo.test
print(foo())