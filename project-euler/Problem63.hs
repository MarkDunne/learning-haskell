import Utils
solution = length [1 | base <- [1..9], power <- [1..21], numDigits (base ^ power) == power]

main = print solution
