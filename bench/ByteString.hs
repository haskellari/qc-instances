module Main where

import qualified Data.ByteString                      as BS
import           System.IO                            (hPutStrLn, stderr)
import qualified Test.QuickCheck                      as QC
import           Test.QuickCheck.Instances.ByteString ()
import qualified Test.QuickCheck.Monadic              as QCM

-- BS.pack 6.53s
-- current: 0.07s
main :: IO ()
main = QC.quickCheckWith args $ QC.property $ \bsList -> QCM.monadicIO $ do
    QCM.run $ hPutStrLn stderr (show $ sum $ map BS.length bsList)
    QCM.assert True
  where
    args = QC.stdArgs { QC.maxSize = 1000 }
