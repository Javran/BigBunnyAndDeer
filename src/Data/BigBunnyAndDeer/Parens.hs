module Data.BigBunnyAndDeer.Parens
  ( lParens
  , rParens
  , parens
  , getLParen
  , getRParen
  , getSomeLParens
  ) where

import Control.Monad.Random
import Data.List.Split
import Control.Monad

import Data.BigBunnyAndDeer.Util

parenList :: String
parenList =
    "()[]{}（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»\
    \「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙\
    \｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱\
    \❲❳⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸"

parens :: [(Char,Char)]
parens = map breakList (chunksOf 2 parenList)
  where
    breakList [a,b] = (a,b)

lParens, rParens :: [] Char
lParens = map fst parens
rParens = map snd parens

getLParen, getRParen :: (Functor m, MonadRandom m) => m Char
getLParen = choice lParens
getRParen = choice rParens

getSomeLParens :: (Functor m, MonadRandom m) => m String
getSomeLParens = do
    n <- getRandomR (0,4)
    replicateM n getLParen
