antall_pers = int(input("Hvor mange er dere på laget?\n"))
antall_twist = int(input("Hvor mange twist er det i posen dere vant?\n"))
print(f"Det blir {antall_twist//antall_pers} twist til hver, og det blir {antall_twist%antall_pers} twist til overs.")
