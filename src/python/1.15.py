# SICP Exercise 1.15

def counted(func):
    count = 0
    def wrapper(*args, **kwargs):
        count += 1
        return func(*args, **kwargs)

    return wrapper

@counted
def add( a, b, called):
    return (a1 + a2, c1 + c2)

@counted
def fib_(n):
    if n == 0:  return 0
    if n == 1:  return 1
    else:       return fib_(n-1) + fib_(n-2)

def fib(n):
    return fib_(n)

def print_fib( n, f, c ):
    print 'fib(%i)=%i, called %i times.' % (n, f, c)

#####################################################################
## 

    



if __name__ == '__main__':
    print [fib(n) for n in range(10)]
