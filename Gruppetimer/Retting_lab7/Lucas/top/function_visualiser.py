import matplotlib.pyplot as plt

def plot_polynomial(a, b, c, xs):
    g = []
    for x in xs:
        f = a*x**2 + b * x + c
        g.append(f)
    plt.suptitle(f'f(x) = {coefficient(a)}x^2{coefficient(b)}x{coefficient(c)}')
    plt.ylabel('f(x)')
    plt.xlabel('x')
    return(plt.plot(xs, g))

def coefficient(co):
    if co == 1:
        return " + "
    elif co == -1:
        return "-"
    elif co < 0:
        return f" - {abs(co)}"
    elif co == 0:
        return None
    elif co >= 0:
        return f' + {abs(co)}'
    
    


if __name__ == "__main__":
    (plot_polynomial(1, -5, 100, [-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10]))
    plt.show() 


