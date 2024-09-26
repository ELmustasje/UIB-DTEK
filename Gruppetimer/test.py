start_population = int(input("Befolkning: "))
growth_rate = float(input("Årlig vekstrate (i prosent): "))
number_of_years = int(input("Antall år: "))


def population_growth(start_population, growth_rate, number_of_years):
    population = start_population

    for year in range(1, number_of_years + 1):
        population = int(population * (1 + (growth_rate / 100)))
        print(f"Befolkningen etter {year} år er {population}")

    total_growth_percentage = ((population - start_population) / start_population) * 100
    print(
        f"Total vekst etter {number_of_years} år er (i prosent) {total_growth_percentage:.1f}%"
    )


population_growth(start_population, growth_rate, number_of_years)
