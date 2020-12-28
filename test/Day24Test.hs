module Day24Test (tests) where

import Day24
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

input =
  "sesenwnenenewseeswwswswwnenewsewsw\n\
  \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
  \seswneswswsenwwnwse\n\
  \nwnwneseeswswnenewneswwnewseswneseene\n\
  \swweswneswnenwsewnwneneseenw\n\
  \eesenwseswswnenwswnwnwsewwnwsene\n\
  \sewnenenenesenwsewnenwwwse\n\
  \wenwwweseeeweswwwnwwe\n\
  \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
  \neeswseenwwswnwswswnw\n\
  \nenwswwsewswnenenewsenwsenwnesesenew\n\
  \enewnwewneswsewnwswenweswnenwsenwsw\n\
  \sweneswneswneneenwnewenewwneswswnese\n\
  \swwesenesewenwneswnwwneseswwne\n\
  \enesenwswwswneneswsenwnewswseenwsese\n\
  \wnwnesenesenenwwnenwsewesewsesesew\n\
  \nenewswnwewswnenesenwnesewesw\n\
  \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
  \neswnwewnwnwseenwseesewsenwsweewe\n\
  \wseweeenwnesenwwwswnew"

tests =
  testGroup
    "Day24"
    [ testCase "solveA" $ solveA input @?= "10"
    -- testCase "solveB" $ solveB input @?= "2208"
    ]
