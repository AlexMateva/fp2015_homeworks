-- Note from lecture
xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

-- Checks if a given number is prime
isPrime :: Int -> Bool
isPrime x
    |x <= 0 = False
    |otherwise = primeloop x 2
        where primeloop n i
                |i == n = True
                |mod n i == 0 = False
                |otherwise = primeloop n (i + 1)

-- Checks if a number is prime, then recursively checks the number without its last digit
isTruncatablePrime :: Int -> Bool
isTruncatablePrime x
    |x < 1 = True
    |not (isPrime x) = False
    |otherwise = isTruncatablePrime (quot x 10)

-- Checks if all digits from the second parameter are contained in the first
containsDigits :: Integer -> Integer -> Bool
containsDigits x y
    |y == 0 = True
    |not (xContains x (mod y 10)) = False
    |otherwise = containsDigits x (quot y 10)
        where
            xContains x' y'
                |x' < 1 = False
                |mod x' 10 == y' = True
                |otherwise = xContains (quot x' 10) y'

-- Finds the product of digits of a number
productOfDigits :: Int -> Int
productOfDigits x
    |x == 0 = 1
    |otherwise = mod x 10 * productOfDigits (quot x 10)

-- Checks whether a number is 'interesting'
interestingNumber :: Int -> Bool
interestingNumber x
    |x < 1 = False
    |otherwise = divisors k == x
        where
            divisors k' =
                sum(filter (\x' -> mod k' x' == 0) [1..(k'-1)])
            k = divisors x

-- Checks quadrant belonging of a point
quadrant :: Double -> Double -> Int
quadrant 0 0 = 0
quadrant x y
    |x > 0 && y > 0 = 1
    |x < 0 && y > 0 = 2
    |x < 0 && y < 0 = 3
    |otherwise = 4
