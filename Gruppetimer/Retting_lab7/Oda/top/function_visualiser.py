import matplotlib.pyplot as plt

def plot_polynomial (a, b, c, xs):
    title = f'f(x) = {coeff_string(a)}x**2{coeff_string(b)}x{coeff_string(c)}'
    x_axis = 'x'
    y_axis = 'f(x)'

    ys = []
    for m in xs:
        y = a * (m**2) + b*m + c
        ys.append(y)
    
    plt.plot(xs, ys)
    plt.xlabel(x_axis)
    plt.ylabel(y_axis)
    plt.title(title)

def coeff_string(coefficient):
    if coefficient == 1:
        return '+'
    elif coefficient == 0:
        return None
    elif coefficient >= 0:
        return f'+ {coefficient}'
    elif coefficient < 0:
        return f'+ {abs(coefficient)}'


if __name__ == '__main__':
    plot_polynomial(1, -5, 100, [-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
    plt.show()