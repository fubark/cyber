use math

nouns := []str{'World', '世界', 'दुनिया', 'mundo'}
nouns <<= math.random().fmt()
for nouns |n|:
    print('Hello, %{n}!')
