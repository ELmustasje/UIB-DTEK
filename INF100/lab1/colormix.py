def getRGB(color):
    r = color // 1000000
    g = (color // 1000) % 1000
    b = color % 1000
    return r, g, b


def blend(colorA, colorB, ratioB):
    ratioA = 1 - ratioB
    rA, gA, bA = getRGB(colorA)
    rB, gB, bB = getRGB(colorB)

    new_r = round(rA * ratioA + rB * ratioB)
    new_g = round(gA * ratioA + gB * ratioB)
    new_b = round(bA * ratioA + bB * ratioB)

    new_color = new_r * 1000000 + new_g * 1000 + new_b
    return new_color


g_farge = int(input("Grunnfarge: "))
m_farge = int(input("Målfarge: "))
ratioB = float(input("Andel målfarge: "))

new_color = blend(g_farge, m_farge, ratioB)
print(new_color)
