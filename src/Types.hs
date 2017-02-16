{-# LANGUAGE DeriveAnyClass, DeriveGeneric, UnicodeSyntax #-}
module Types
    (
        ФорматРазметки(..),
        Paper(..)
    ) where

{--
     | | | | ||
     | | | | ||
     | | | | ||
     | | | | ||
    =|=|=|=|=||

    _|_|_|_|_||
    _|_|_|_|_||
    _|_|_|_|_||
    _|_|_|_|_||
    =|=|=|=|=||

--}
data ФорматРазметки = MMLines | MMQuadros
data Paper = A4 | A5
