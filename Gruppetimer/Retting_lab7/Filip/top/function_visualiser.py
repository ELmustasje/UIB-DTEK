import matplotlib.pyplot as plt


def coeff_string(coefficient, variable=''):
    if coefficient == 0:
        return ""
    elif coefficient == 1:
        return f' + {variable}' if variable else '+ 1'
    elif coefficient == -1:
        return f' - {variable}'if variable else '- 1'
    elif coefficient > 0:
        return f'+{coefficient}{variable}'
    else:
        return f'- {abs(coefficient)}{variable}'


def plot_polynomial(a, b, c, xs):
    ys = [a * x**2 + b * x +c for x in xs]

    plt.plot(xs, ys, label=f'f(x) = {a}x^2 + {b}x + {c}')

    plt.xlabel('x')
    plt.ylabel('f(x)')

    title = f"f(x) = {coeff_string(a, 'x^2')} {coeff_string(b, 'x')} {coeff_string(c)}".strip()
    plt.title(title.replace("+ -", "- ").replace("f(x) = + ", "f(x) = "))

if __name__ == "__main__":
    plot_polynomial(1, -5, 100, [-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
    plt.show()  # ha gjerne plt.show() utenfor plot_polynomial

