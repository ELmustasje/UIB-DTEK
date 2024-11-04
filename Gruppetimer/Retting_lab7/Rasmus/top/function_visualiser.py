from matplotlib import pyplot as plt


def plot_polynomial(a, b, c, xs):
    for x in xs:
        ys = []
        for x in xs:
            ys.append(a * x**2 + b * x + c)

    plt.plot(xs, ys)

    plt.xlabel(
        "x",
        fontsize="large",
    )

    a_print = str(abs(a))
    ax = "x^2 "
    b_print = str(abs(b))
    bx = "x "
    c_print = str(abs(c))

    if a > 0 and a != 1:
        a_sign = "+ "
    elif a < 0:
        a_sign = "- "
    elif a == 1:
        a_print = ""
        a_sign = "+ "
    elif a == 0:
        a_print = ""
        ax = ""
        a_sign = ""

    if b > 0 and b != 1:
        b_sign = "+ "
    elif b < 0:
        b_sign = "- "
    elif b == 1:
        b_print = ""
        b_sign = "+ "
    elif b == 0:
        b_print = ""
        bx = ""
        b_sign = ""

    if c > 0:
        c_sign = "+ "
    elif c < 0:
        c_sign = "- "
    elif c == 0:
        c_print = ""
        c_sign = ""

    plt.ylabel("f(x)")
    fontsize = "large"
    plt.title(
        "f(x) = " + a_sign + a_print + ax + b_sign + b_print + bx + c_sign + c_print
    )
    plt.show()


if __name__ == "__main__":
    plot_polynomial(1, -5, 100, [-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10])
    plt.show()  # ha gjerne plt.show() utenfor plot_polynomial
