import matplotlib.pyplot as plt 
def plot_polynomial (a, b, c, xs):
    ys = [a* x**2 + b*x + c for x in xs]

    plt.plot(xs, ys, label='f(x)')
    
    plt.xlabel('x')
    plt.ylabel('f(x)')

    title = f'f(x) = +{a}x^2' + (f'+ {b}x' if b > 0 else f'- {-b}x +{c}')
    plt.title(title)

if __name__ == "__main__":
    plot_polynomial(1, -5, 100, [-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
    plt.show()  # ha gjerne plt.show() utenfor plot_polynomial
