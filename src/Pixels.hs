module Pixels where

import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture

import Misc
import List

white = (255, 255, 255)
red = (255, 0, 0)
blue = (0, 0, 255)
green = (0,255,0)

pixelsToPng :: FilePath -> Int -> Int -> [[[Double]]] -> IO ()
pixelsToPng dest sx sy l = do
  let l' = (((round<$>)<$>)<$>) l
      colours = [red, blue, green, (255,0,255), (255,255,0), (0, 255, 255)]
      f l x y = px . fromJust . just_or_default (Just white) . safe_head . filter (/=Nothing) $ zipWith f' colours l
        where f' col l = if any (\[a, b] -> a==x && b==y) l
                         then Just col
                         else Nothing
              px (r, g, b) = PixelRGB8 r g b
  savePngImage dest $ ImageRGB8 $ generateImage (f l') sx sy



