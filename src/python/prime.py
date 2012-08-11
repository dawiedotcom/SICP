#!/usr/bin/python

from random import randint

def expmod(base, exp, m):
    if not exp:
        return 1
    elif not exp%2:
        return (expmod(base, exp/2, m)**2) % m
    else:
        return (base * expmod(base, exp-1, m)) % m

def fermat_test(n):
    def try_it(a):
        return expmod(a, n, n) == a
    return try_it(randint(1, n-1))


def isPrime(n, times):
    if not times:
        return True
    elif fermat_test(n):
        return isPrime(n, times-1)
    else:
        return False

if __name__=='__main__':
    _range = xrange(2,1000)
    print filter(lambda x: isPrime(x, 15), _range)
