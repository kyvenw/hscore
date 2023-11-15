import qualified EuterpeaConverterTest
import qualified LilyPondPostProcessTest
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running Euterpea Converter tests:"
  EuterpeaConverterTest.runTests
  putStrLn "Running Lilypond Post Process tests:"
  LilyPondPostProcessTest.runTests
  putStrLn "Done!"
