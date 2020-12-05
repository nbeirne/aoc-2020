import Data.List.Split
import Data.List
import Text.Regex
import Text.Read

req = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

valid1 lst = all hasKey req
  where hasKey k = length (filter (\(k',_) -> k == k') lst) == 1 

solve1 = length . filter valid1 


-- part 2

isBounded min max (Just i) = i >= min && i <= max 
isBounded _ _ _ = False

height s = height' unit $ readMaybe $ reverse $ drop 2 $ reverse s
  where unit = reverse $ take 2 $ reverse s
        --num = reverse $ drop 2 $ reverse s
        height' "cm" s = isBounded 150 193 s
        height' "in" s = isBounded 59 76 s
        height' _ _ = False

validate "byr" s = isBounded 1920 2002 $ readMaybe s
validate "iyr" s = isBounded 2010 2020 $ readMaybe s
validate "eyr" s = isBounded 2020 2030 $ readMaybe s
validate "hgt" s = height s
validate "hcl" s = matchRegex (mkRegex "^#[0-9a-f]{6}$") s /= Nothing
validate "ecl" s = s `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
validate "pid" s = matchRegex (mkRegex "^[0-9]{9}$") s /= Nothing


valid2 lst = all hasKey req
  where hasKey k = length (filter (\(k',v) -> k == k' && validate k v) lst) == 1 

solve2 = length . filter valid2

-- parse

parsel s = (sp !! 0, sp !! 1)
  where sp = splitOn ":" s

parse c = map (map parsel) $ map (splitOn " " . intercalate " ") (splitOn [""] $ lines c)

-- boiler plate

run f = do
  c <- readFile "input"
  --mapM_ putStrLn $ map show $ parse $ c
  putStrLn $ show $ f $ parse $ c

main = run solve1 >> run solve2 >> return ()
