{-# LANGUAGE PatternGuards #-}

-- | Module for dealing with escape codes
module Ghcid.Escape
  ( WordWrap (..)
  , Esc (..)
  , unescape
  , stripInfixE
  , stripPrefixE
  , isPrefixOfE
  , spanE
  , trimStartE
  , unwordsE
  , unescapeE
  , wordWrapE
  )
where

import qualified Data.Char as Char
import qualified Data.Either.Extra as Either.Extra
import qualified Data.String as String
import qualified Data.Tuple.Extra as Tuple.Extra

-- A string with escape characters in it
newtype Esc = Esc { fromEsc :: String }
  deriving newtype (Eq, Semigroup)

app :: Esc -> Esc -> Esc
app = (<>)

unesc :: Esc -> Maybe (Either Esc Char, Esc)
unesc (Esc ('\ESC':xs)) | (pre,'m':post) <- break (== 'm') xs = Just (Left $ Esc $ '\ESC':pre<>"m", Esc post)
unesc (Esc (x:xs)) = Just (Right x, Esc xs)
unesc (Esc []) = Nothing

explode :: Esc -> [Either Esc Char]
explode = unfoldr unesc

implode :: [Either Esc Char] -> Esc
implode = Esc . concatMap (either fromEsc pure)

unescape :: String -> String
unescape = unescapeE . Esc

-- | Remove all escape characters in a string
unescapeE :: Esc -> String
unescapeE = rights . explode

stripPrefixE :: String -> Esc -> Maybe Esc
stripPrefixE [] e = Just e
stripPrefixE (x:xs) e = case unesc e of
  Just (Left code, rest) -> app code <$> stripPrefixE (x:xs) rest
  Just (Right y, rest) | y == x -> stripPrefixE xs rest
  _ -> Nothing

stripInfixE :: String -> Esc -> Maybe (Esc, Esc)
stripInfixE needle haystack
  | Just rest <- stripPrefixE needle haystack = Just (Esc [], rest)
stripInfixE needle e =
  case unesc e of
    Nothing ->
      Nothing
    Just (x,xs) ->
      first (app $ Either.Extra.fromEither $ fmap (Esc . pure) x) <$> stripInfixE needle xs

breakE :: (Char -> Bool) -> Esc -> (Esc, Esc)
breakE f = spanE (not . f)

spanE :: (Char -> Bool) -> Esc -> (Esc, Esc)
spanE f e = case unesc e of
  Nothing ->
    (Esc "", Esc "")
  Just (Left e', rest) ->
    first (app e') $ spanE f rest
  Just (Right c, rest)
    | f c -> first (app $ Esc [c]) $ spanE f rest
    | otherwise -> (Esc "", e)

isPrefixOfE :: String -> Esc -> Bool
isPrefixOfE x y = isJust $ stripPrefixE x y

trimStartE :: Esc -> Esc
trimStartE e =
  case unesc e of
    Nothing -> Esc ""
    Just (Left code, rest) -> app code $ trimStartE rest
    Just (Right c, rest)
      | Char.isSpace c -> trimStartE rest
      | otherwise -> e

unwordsE :: [Esc] -> Esc
unwordsE = Esc . String.unwords . map fromEsc


repeatedlyE :: (Esc -> (b, Esc)) -> Esc -> [b]
repeatedlyE _ (Esc []) = []
repeatedlyE f as = b : repeatedlyE f as'
    where (b, as') = f as

splitAtE :: Int -> Esc -> (Esc, Esc)
splitAtE i e =
  case unesc e of
    _ | i <= 0 -> (Esc "", e)
    Nothing ->
      (e, e)
    Just (Left code, rest) ->
      first (app code) $ splitAtE i rest
    Just (Right c, rest) ->
      first (app $ Esc [c]) $ splitAtE (i-1) rest

reverseE :: Esc -> Esc
reverseE = implode . reverse . explode

breakEndE :: (Char -> Bool) -> Esc -> (Esc, Esc)
breakEndE f = swap . Tuple.Extra.both reverseE . breakE f . reverseE


lengthE :: Esc -> Int
lengthE = length . unescapeE


-- | 'WrapHard' means you have to
data WordWrap
  = WrapHard
  | WrapSoft
  deriving stock (Eq, Show)


-- | Word wrap a string into N separate strings.
--   Flows onto a subsequent line if less than N characters end up being empty.
wordWrapE :: Int -> Int -> Esc -> [(Esc, WordWrap)]
wordWrapE mx gap = repeatedlyE \x ->
  let
    (a,b) = splitAtE mx x
    (a1,a2) = breakEndE Char.isSpace a
  in
    if b == Esc "" then
      ((a, WrapHard), Esc "")
    else if lengthE a2 <= gap then
      ((a1, WrapHard), app a2 b)
    else
      ((a, WrapSoft), trimStartE b)
