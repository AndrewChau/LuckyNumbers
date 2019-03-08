# Lucky Numbers

## Running Solution

`fsharpi solution.fsx [number to check]`


# Nearest lucky numbers

__Challenge from:__ https://old.reddit.com/r/dailyprogrammer/comments/6wjscp/2017828_challenge_329_easy_nearest_lucky_numbers

A [Lucky Number](https://en.wikipedia.org/wiki/Lucky_number) is a natural number in a set which is generated by a certain "sieve". This sieve is similar to the Sieve of Eratosthenes that generates the primes, but it eliminates numbers based on their position in the remaining set, instead of their value (or position in the initial set of natural numbers).

The set of lucky numbers can be obtained by:-

Begin with a list of integers starting with 1:
```
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
```
Starting with 1, remove every other element (i.e. every even number) from this set. We are left with:
```
1 3 5 7 9 11 13 15 17 19 21 23 25
```
After 1, the next number in the set is 3. So, remove every 3rd number. Clearly, 5 is removed because it's the third number in the above set. Go on and keep removing every 3rd number.

Your new set is:
```
1 3 7 9 13 15 19 21 25...
```
Here, the next remaining number you have after 3 is 7. Now, at this point, it's obvious that there's no way 1 and 3 are ever getting eliminated. Thus, we can conclude that 1 and 3 are lucky numbers.

Now remove every 7th number. Clearly, 19 would be the first to be wiped out.

You're left with:
```
1 3 7 9 13 15 21 25 ...
```
Now it's time to proceed to the next remaining number after 7, i.e., 9. Remove every 9th number. Note that at this point, 7 cannot be eliminated. 7 is a lucky number too.

Keep proceeding in a similar fashion in order to get a list of lucky numbers.

Numbers remaining after this procedure has been carried out completely are called lucky numbers. The first few are
```
1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, ...
```
Today's challenge is to find the nearest lucky number. This might remind you of Challenge #326. In fact, this has been inspired from it. Bruteforcing is the most obvious option, but it's certainly not the most efficient.

## Input Description
The input will be a positive integer.

## Output Description
The output will be
```
previous lucky number < n < next lucky number
```
where _n_ is your input.

If _n_ is a lucky number, then display
```
n is a lucky number.
```

## Challenge Input
```
103

225

997
```
## Challenge Output
```
99 < 103 < 105

223 < 225 < 231

997 is a lucky number
```

## Bonus
Find every lucky number all the way up to 10,000,000 and post your the time it took to run. This is so that you can compete amongst everyone else to see who has the most efficient one.
