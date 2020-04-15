main = do 
  putStrLn "Exercicios Aula 8 - Paradigmas"

--questão 1
isBin :: String -> Bool
isBin "" = False  
isBin string = if head string == '0' || head string == '1' 
  then if tail string == ""
    then True
    else isBin (tail string) 
  else False

--questão 2
isBin1 :: String -> Bool
isBin1 "" = False
isBin1 string = if (filter (not.(\c-> elem c "01")) string) == "" 
  then True 
  else False 

--questão 3
auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec bits expo = if length bits == 0 
  then 0
  else ((head bits) * 2 ^ expo) + auxBin2Dec (tail bits) (expo-1)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

--questão 4
bin2dec' :: [Int] -> Int 
bin2dec' [] = undefined 
bin2dec' bits = sum (zipWith (\bit expo -> bit*2^expo) [bit | bit <- bits] [expo | expo <- [length bits..1]])

--questão 5
dec2bin :: Int -> [Int]
dec2bin 0 = []
dec2bin n = dec2bin (div n 2) ++ [mod n 2] 

--questão 6
isHex :: String -> Bool
isHex "" = False 
isHex string = if elem (head string) "ABCDEF123456789"
  then if tail string == ""
    then True
    else isHex (tail string) 
  else False