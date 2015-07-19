module Encode where

import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Put
import Data.Int
import Types

encodeAndWrite :: FilePath -> Samples -> IO ()
encodeAndWrite path = LBS.writeFile path . runPut . encode

encode :: Samples -> Put
encode (Samples ss) = mapM_ encodeSample ss

encodeSample :: Sample -> Put
encodeSample (Sample f) =
  let
    limit = fromIntegral (maxBound :: Int16)
    i16 = round ((min 1 . max (-1)) f * limit) :: Int16
  in putWord16le $ fromIntegral i16
