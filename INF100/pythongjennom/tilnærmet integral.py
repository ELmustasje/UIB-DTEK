from math import*

def f(x):
    return 9.5+10.5*sin(0.45*x-1.129)

a = 1 ##nedregrense
b = 3 ##øvregrense
n = 100 ##antall
delta_x = (b-a)/n
sum_rek = 0
sum_trap = 1/2*f(a)*delta_x
startpos = 0 * delta_x
## 0 = venstra, 0.5 midt, 1 høyre

for i in range(1,n+1): ##rektangelmetode
    sum_rek += f(1+(i-1)*delta_x+startpos)*delta_x

for i in range(2,n+1):
    sum_trap += f(a+(i-1)*delta_x)*delta_x
sum_trap += 1/2*f(b)*delta_x

print(sum_rek)
print(sum_trap)