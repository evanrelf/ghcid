{-# LANGUAGE PatternGuards, ViewPatterns, TupleSections #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Parses the output from GHCi
module Ghcid.Parser(
    parseShowModules, parseShowPaths, parseLoad
    ) where

import qualified Data.Char as Char
import qualified Data.List.Extra as List

import Ghcid.Types
import Ghcid.Escape


-- | Parse messages from show modules command. Given the parsed lines
--   return a list of (module name, file).
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules (map unescape -> xs) =
    -- we only return raw values, don't want any escape codes in there
    [ (takeWhile (not . Char.isSpace) $ List.trimStart a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]

-- | Parse messages from show paths command. Given the parsed lines
--   return (current working directory, module import search paths)
parseShowPaths :: [String] -> (FilePath, [FilePath])
parseShowPaths (map unescape -> xs)
    | (_:x:_:is) <- xs = (List.trimStart x, map List.trimStart is)
    | otherwise = (".",[])

-- | Parse messages given on reload.
parseLoad :: [String] -> [Load]
-- nub, because cabal repl sometimes does two reloads at the start
parseLoad (map Esc -> xs) = List.nubOrd $ f xs
    where
        f :: [Esc] -> [Load]

        -- [1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )
        f (xs:rest)
            | Just xs <- stripPrefixE "[" xs
            = map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') $ unescapeE xs]) ++
              f rest

        -- GHCi.hs:81:1: Warning: Defined but not used: `foo'
        f (x:xs)
            | not $ " " `isPrefixOfE` x
            , Just (file,rest) <- breakFileColon x
             -- take position, including List.span if present
            , Just ((pos1, pos2), rest) <- parsePosition rest
            , (msg,las) <- List.span isMessageBody xs
            , rest <- trimStartE $ unwordsE $ rest : xs
            , sev <- if "warning:" `isPrefixOf` List.lower (unescapeE rest) then Warning else Error
            = Message sev file pos1 pos2 (map fromEsc $ x:msg) : f las

        -- <no location info>: can't find file: FILENAME
        f (x:xs)
            | Just file <- stripPrefixE "<no location info>: can't find file: " x
            = Message Error (unescapeE file) (0,0) (0,0) [fromEsc x] : f xs

        -- <no location info>: error:
        f (x:xs)
            | unescapeE x == "<no location info>: error:"
            , (xs,rest) <- List.span leadingWhitespaceE xs
            = Message Error "<unknown>" (0,0) (0,0) (map fromEsc $ x:xs) : f rest

        -- Module imports form a cycle:
        --   module `Module' (Module.hs) imports itself
        f (x:xs)
            | unescapeE x == "Module imports form a cycle:"
            , (xs,rest) <- List.span leadingWhitespaceE xs
            , let ms = [takeWhile (/= ')') x | x <- xs, '(':x <- [dropWhile (/= '(') $ unescapeE x]]
            = [Message Error m (0,0) (0,0) (map fromEsc $ x:xs) | m <- List.nubOrd ms] ++ f rest

        -- Loaded GHCi configuration from C:\Neil\ghcid\.ghci
        f (x:xs)
            | Just x <- stripPrefixE "Loaded GHCi configuration from " x
            = LoadConfig (unescapeE x) : f xs

        f (_:xs) = f xs
        f [] = []

leadingWhitespaceE :: Esc -> Bool
leadingWhitespaceE x =
    isPrefixOfE " " x || isPrefixOfE "\t" x

-- 1:2:
-- 1:2-4:
-- (1,2)-(3,4):
parsePosition :: Esc -> Maybe (((Int, Int), (Int, Int)), Esc)
parsePosition x
    | Just (l1, x) <- digit x, Just x <- lit ":" x, Just (c1, x) <- digit x = case () of
        _ | Just x <- lit ":" x -> Just (((l1,c1),(l1,c1)), x)
          | Just x <- lit "-" x, Just (c2,x) <- digit x, Just x <- lit ":" x -> Just (((l1,c1),(l1,c2)), x)
          | otherwise -> Nothing
    | Just (p1, x) <- digits x, Just x <- lit "-" x, Just (p2, x) <- digits x, Just x <- lit ":" x = Just ((p1,p2),x)
    | otherwise = Nothing
    where
        lit = stripPrefixE

        digit x = (,b) <$> readMaybe (unescapeE a)
            where (a,b) = spanE Char.isDigit x

        digits x =  do
            x <- lit "(" x
            (l,x) <- digit x
            x <- lit "," x
            (c,x) <- digit x
            x <- lit ")" x
            pure ((l,c),x)


-- After the file location, message bodies are indented (perhaps prefixed by a line number)
isMessageBody :: Esc -> Bool
isMessageBody xs = isPrefixOfE " " xs || case stripInfixE "|" xs of
  Just (prefix, _) | all (\x -> Char.isSpace x || Char.isDigit x) $ unescapeE prefix -> True
  _ -> False

-- A filename, followed by a colon - be careful to handle Windows drive letters, see #61
breakFileColon :: Esc -> Maybe (FilePath, Esc)
breakFileColon xs = case stripInfixE ":" xs of
    Nothing -> Nothing
    Just (a,b)
        | [drive] <- unescapeE a, Char.isLetter drive -> first ((++) [drive,':'] . unescapeE) <$> stripInfixE ":" b
        | otherwise -> Just (unescapeE a, b)
