def can_be_made_of_letters(word, letters):
    word = list(word)
    letters = list(letters)
    wildcards = 0
    wrong = 0

    for i in word:
        if i not in letters:
            wrong += 1
        else:
            letters.remove(i)

    for i in letters:
        if i == "*":
            wildcards += 1

    if wildcards >= wrong:
        return True
    else:
        return False


def possible_words(wordlist, letters):
    funnet = []
    for word in wordlist:
        if can_be_made_of_letters(word, letters):
            funnet.append(word)
    return funnet
