{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Codec.BMP(bmpDimensions, packRGBA32ToBMP, packRGBA32ToBMP24, readBMP, writeBMP, unpackBMPToRGBA32, BMP(..))
import Data.List (concatMap, sort, group)
import qualified Data.Vector.Storable as DVS ((++), Vector, concat, fromList, replicate)
import Data.Word8 as W8
import qualified Data.ByteString as BS (ByteString, pack, take, unpack, group, split)
import Types(Paper(..), ФорматРазметки(..))
import Codec.Picture as CP (DynamicImage(..), Image(..), Pixel8, PixelRGB8(..), generateImage, readImage, savePngImage, writePng)
import Codec.Picture.Types (PixelBaseComponent(..), dropTransparency, convertPixel, imageWidth, imageHeight)
-- Для генерации клеток в BMP:
-- Формат A4 в пикселях при 300 dpi
formatA4 = (2480, 3508)
width = fst
height = snd
widthA4 = width formatA4
heightA4 = height formatA4
widthA5 = 1754
heightA5 = widthA4
-- convc:: (Pixel a) => [PixelRGB8] -> DVS.Vector (PixelBaseComponent a)
-- convc xs = (fromList xs) ::DVS.Vector (Word8)
-- Цвета линий
base = [255, 255, 255]          -- фон
l1c = [220, 220, 255]           -- 1 мм шаг
l5c = [210, 210, 255]           -- 5 мм шаг
l1cf = [240, 255, 255]           -- 1 мм шаг на полях
l5cf = [240, 245, 255]           -- 5 мм шаг на полях
cldiv = [250, 230, 255]             -- разрез

mm1 = 12
mm5 = 59
hnOfmm5 = widthA5 `div` mm5
-- Размеры полей в пикселях
ft = 2*mm5-l5                        -- верхнее
fl = 2*mm5                        -- левое
fb = mm5-l5                          -- нижнее
fr = mm5-l5                          -- правое

-- Толщина линий в пикселях
l1 = 3
l5 = 5
a1 = 3      -- Количество повторений разметки с шагом 1 мм
lb = 3      -- Количество Word8 в пикселе

line_px = lb*widthA4 -- количество пикселей в строке
tc x = take x .cycle
tcLb u = tc (lb * u)
d1 = mm1 - l1
d1b5 = mm1 - l5 -- дистанция между 1 мм и 5 мм линиями o o o o O
d5 = 5 * mm1 - l5
cr x = concat . replicate x

cm :: (Foldable t0) => t0 (Int, [b]) -> [b]
cm = concatMap (\(x, y) -> cr x y)
ze x = cm [(d1, base), (l1, x)]
-- Поля
-- нижнее
lft = (d1b5, base)
-- Для миллиметровой разметки в A4
{-
 | | | | ||
-}
lineStep11115 = tc line_px $ cm [(d1b5, base), (l1, l1c), (a1, ze l1c), (d1b5, base), (l5, l5c)]
ulineStep11115first c1 c5 = cm [(d1b5, base), (l1, c1), (a1, ze c1), (d1b5, base)]
ulineStep11115 c1 c5 = cm [(d1b5, base), (l1, c1), (a1, ze c1), (d1b5, base), (l5, c5)]
lineFieldFull = tc line_px $ ulineStep11115 l1cf l5cf
lineMidFull = take line_px $ (ulineStep11115first l1cf l5cf) ++ cr l5 l5c ++ (cr ((2480 `div` mm5)-2) $ cm [(d1b5, base), (l1, l1c), (a1, ze l1c), (d1b5, base), (l5, l5c)]) ++ (cr 2 $ ulineStep11115 l1cf l5cf)
{-
_|_|_|_|_||
-}
vertLinesField = cm [(d5, lineFieldFull), (l5, line5gFields l5cf), (d5, lineFieldFull)] -- поле слева
vertLinesMid = cm [(d5, lineMidFull), (l5, line5gFields l5c)]
vfull = vertLinesField ++ cm [(l5, line5gFields l5c), (hnOfmm5-3, vertLinesMid), (d5, lineFieldFull)]
pageA5full = vfull ++ cr 18 (line5G base)
{-
    _|_|_|_|_||
    _|_|_|_|_||
    _|_|_|_|_||
    _|_|_|_|_||
    =|=|=|=|=||

vertLineFull = cm [(d1b5, lineStep11115), (l1, line1G), (a1, vertLine1w1b), (d1b5, lineStep11115), (l5, line5G)]
-}
{-
    Для смешанной разметки - шаг по вертикали 5 мм
     | | | | ||
     | | | | ||
     | | | | ||
     | | | | ||
    =|=|=|=|=||
    
vertLines5Full = cm [(d5, lineStep11115), (l5, (line5G l5c))]

-- Для обычной клетки 5 мм (tc4 d5 base)
-}
lineStep5 x = tc line_px $ x ++ (cr l5 l5c)     -- Для 5 мм клетки
{-
vertLineStep5 = tc (d5*widthA4) (lineStep5 (tc4 d5 base)) ++ tc (l5*widthA4) (line5G l5c)

line1G = tc4 lengthLine l1c
-}
line5G c = tc line_px c
line5gFields c = tc line_px $ cm [(fb, l5cf), (39*mm5+23, c), (ft, l5cf)]
pl4 ll = print $ (length ll) `div` lb


-- Для A5
fA5lineStep11115 c1 c5 = tcLb widthA4 $ cm [(d1b5, base), (l1, c1), (a1, ze c1), (d1b5, base), (l5, c5)]
-- ftA5linesStep5 = cr 4 (fA5lineStep11115 l1cf l5cf)

imageCreator :: String -> IO ()
imageCreator path = do
    let hh = CP.writePng path
    hh $ generateImage pixelRenderer widthA4 300
    where pixelRenderer x y = PixelRGB8 255 255 255 -- (fromIntegral x) (fromIntegral y)

transcodeToPng :: FilePath -> FilePath -> IO ()
transcodeToPng pathIn pathOut = do
   eitherImg <- readImage pathIn
   case eitherImg of
       Left _ -> return ()
       Right img -> savePngImage pathOut img

main ∷ IO ()
main = do
    let vv = DVS.fromList $ vfull ++ cr 18 (line5G base) ++ cr 2 (line5G cldiv)++vfull ++ cr 18 (line5G base)
    CP.savePngImage "out/mm.png" $ CP.ImageRGB8 (Image widthA4 (heightA4) vv:: Image PixelRGB8)
