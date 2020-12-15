# play :: Int -> SpokenNumbers -> Int -> Int -> Int
# play target spokenNumbers number turn =
#   if turn == target then nextNumber else f
#   where
#     previousTurn = IntMap.lookup number spokenNumbers
#     nextNumber = case previousTurn of
#       Just turn' -> turn - 1 - turn'
#       Nothing -> 0
#     f = play target (IntMap.insert number (turn - 1) spokenNumbers) nextNumber (turn + 1)

# solve' :: Int -> String -> String
# solve' target s = show $ play target initialSpokenNumbers (last startingNumbers) (length startingNumbers + 1)
#   where
#     startingNumbers = map (\v -> read v :: Int) $ splitOn "," $ head $ lines s
#     initialSpokenNumbers = IntMap.fromList $ zip startingNumbers [1 .. (length startingNumbers - 1)]

# solveA :: String -> String
# solveA = solve' 2020

# loop :: Int -> Int -> String
# loop target acc = if target == acc then "OK" else loop target (acc + 1)

# solveB :: String -> String
# solveB = solve' 30000000
import sys

m = {}

def play(target: int, number: int, turn: int):
  nextNumber = None

  while turn <= target:
    prevTurn = m.get(number)
    nextNumber = 0 if prevTurn is None else turn - 1 - prevTurn
    m[number] = turn - 1

    # print(f"Turn: {turn}, number: {number}, nextNumber: {nextNumber}")

    turn += 1
    number = nextNumber



  return nextNumber



  # return nextNumber if turn == target else play(target, nextNumber, turn + 1)

def solve(input: str):
  numbers = [int(v) for v in input.split(",")]
  for (n, i) in zip(numbers[:-1], range(1, 1 + len(numbers) - 1)):
    m[n] = i

  return play(30000000, numbers[-1], len(numbers) + 1)

print(solve(sys.argv[1]))
