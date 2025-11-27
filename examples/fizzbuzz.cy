-- Fizz Buzz problem from https://code.golf
-- Print the numbers from 1 to 100 inclusive, each on their own line.
-- If, however, the number is a multiple of three then print Fizz instead, and if the number is a multiple of five then print Buzz.
-- If multiple conditions hold true then all replacements should be printed, for example 15 should print FizzBuzz.

for 1..101 |i|:
    if i % 3 == 0 and i % 5 == 0:
        print('FizzBuzz')
    else i % 3 == 0:
        print('Fizz')
    else i % 5 == 0:
        print('Buzz')
    else:
        print(i)