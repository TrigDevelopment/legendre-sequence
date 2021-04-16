import Test.HUnit

main :: IO ()
main = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))