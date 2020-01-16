
import GenTests
import CoTests
import BuysSorterTests

main :: IO ()
main = do
  genTests
  coTests
  buysSorterTests
