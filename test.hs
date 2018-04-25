main = do putStrLn "What is 2 + 2?" -- nicht einfach do nutzen wenn du monaden noch nicht kennst :P LG sascha
          x <- readLn
          if x == 4
                then putStrLn "You're right!"
                else putStrLn "You're wrong!"
-- ein Kommentar

{-
ein
Block
kommentar
-}

-- eine Variablendeklaration
magicNumber :: Int  -- Typ der Variablen
magicNumber = 42    -- Wert der Variablen

magicNumber' = magicNumber + 1

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

listcomp1 = [x*2 | x <- [1..10], x*12 >= 12]

{-breakdown of listcomp1
listcomp1 =     | assign something to listcomp1 function
[x*2 |          | do this to whatever was bound to x
x <- [1..10],   | bind range 1..10 to x
x*12 >= 12]     | check for this condition and leave out any violation -}

listcomp2 = [ x | x <- [50..100], x `mod` 7 == 3]
moduloThree ys = [y | y <- ys, y `mod` 7 == 3]

ranger r = [ x | x <- r, x <= 10] --generate a range below 10 when given a range

checkForOdd y = if ranger y !! 0 `mod` 2 == 0 -- check input for oddity. Can be done with odd keyword
                    then print "Bums!"
                    else print "Bums2!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] -- simple lists checked for oddity and modifiied output

{- -- nested lists checked for even

ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]     | put list into xxs
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]                                            | outer brackets bind each element of xxs to xs while inner brackets check for even inside nested lists 
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]

-}


fac :: Integer -> Integer
fac 0 = 0
fac n = n + fac (n - 1)

fakul v = if v == 0 then 1 else v * (v-1) -- calculates n * (n-1) only

fakul' u = if (u == 0) then 1 else u * fakul' (u-1) -- builds a recursion for n * (n-1) down to n == 0
{-}
Wertet folgendermaßen aus für e.g.

fakul' 3    => 3 * fakul' 2
            => 3 * (2 * fakul' 1)
            => 3 * (2 * (1 * fakul' 0))
            => 3 * (2 * (1 * 1))
            => 3 * (2 * 2)
            => 3 * 4
            => 12
            
-}

fakul2 n = fakulakku n 1
           where fakulakku n akku = if (n == 0) then akku
                                    else fakulakku (n-1) ( n * akku)

length' xs = sum [1 | _ <- xs]  -- takes whatever is bound, replaces it with 1 and sums it up resulting the lentgh of whatever list it is given

removeNonUppercase :: [Char] -> [Char] -- [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] -- checks input against upper case and leaves out lower cases

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n =  product[1..n]