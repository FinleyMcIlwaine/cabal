{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec
  ( Parsec (..)
  , ParsecParser (..)
  , runParsecParser
  , runParsecParser'
  , simpleParsec
  , simpleParsecBS
  , simpleParsec'
  , simpleParsecW'
  , lexemeParsec
  , eitherParsec
  , explicitEitherParsec
  , explicitEitherParsec'
  , parsecHaskellString
  , askCabalSpecVersion

    -- ** Warnings
  , PWarnType (..)
  , PWarning (..)
  , showPWarning
  , parsecWarning

    -- ** Errors
  , PError (..)
  , showPError

    -- * Position
  , Position (..)
  , incPos
  , retPos
  , showPos
  , zeroPos

    -- * Utilities
  , parsecToken
  , parsecToken'
  , parsecFilePath
  , parsecQuoted
  , parsecMaybeQuoted
  , parsecCommaList
  , parsecCommaNonEmpty
  , parsecLeadingCommaList
  , parsecLeadingCommaNonEmpty
  , parsecOptCommaList
  , parsecLeadingOptCommaList
  , parsecStandard
  , parsecUnqualComponentName
  ) where

import Data.ByteString (ByteString)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), showPError)
import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning (PWarnType (..), PWarning (..), showPWarning)
import Numeric (showIntAtBase)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Distribution.Compat.MonadFail as Fail
import qualified Text.Parsec as Parsec

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class for parsing with @parsec@. Mainly used for @.cabal@ file fields.
--
-- For parsing @.cabal@ like file structure, see "Distribution.Fields".
class Parsec a where
  parsec :: ParsecParser a

parsecHaskellString :: ParsecParser String
parsecHaskellString = stringLiteral

-- | 'parsec' /could/ consume trailing spaces, this function /will/ consume.
lexemeParsec :: Parsec a => ParsecParser a
lexemeParsec = parsec <* P.spaces

newtype ParsecParser a = PP
  { unPP
      :: CabalSpecVersion
      -> Parsec.Parsec FieldLineStream [PWarning] a
  }

liftParsec :: Parsec.Parsec FieldLineStream [PWarning] a -> ParsecParser a
liftParsec p = PP $ \_ -> p

instance Functor ParsecParser where
  fmap f p = PP $ \v -> fmap f (unPP p v)
  {-# INLINE fmap #-}

  x <$ p = PP $ \v -> x <$ unPP p v
  {-# INLINE (<$) #-}

instance Applicative ParsecParser where
  pure = liftParsec . pure
  {-# INLINE pure #-}

  f <*> x = PP $ \v -> unPP f v <*> unPP x v
  {-# INLINE (<*>) #-}
  f *> x = PP $ \v -> unPP f v *> unPP x v
  {-# INLINE (*>) #-}
  f <* x = PP $ \v -> unPP f v <* unPP x v
  {-# INLINE (<*) #-}

instance Alternative ParsecParser where
  empty = liftParsec empty

  a <|> b = PP $ \v -> unPP a v <|> unPP b v
  {-# INLINE (<|>) #-}

  many p = PP $ \v -> many (unPP p v)
  {-# INLINE many #-}

  some p = PP $ \v -> some (unPP p v)
  {-# INLINE some #-}

instance Monad ParsecParser where
  return = pure

  m >>= k = PP $ \v -> unPP m v >>= \x -> unPP (k x) v
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance MonadPlus ParsecParser where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail ParsecParser where
  fail = P.unexpected

instance P.Parsing ParsecParser where
  try p = PP $ \v -> P.try (unPP p v)
  p <?> d = PP $ \v -> unPP p v P.<?> d
  skipMany p = PP $ \v -> P.skipMany (unPP p v)
  skipSome p = PP $ \v -> P.skipSome (unPP p v)
  unexpected = liftParsec . P.unexpected
  eof = liftParsec P.eof
  notFollowedBy p = PP $ \v -> P.notFollowedBy (unPP p v)

instance P.CharParsing ParsecParser where
  satisfy = liftParsec . P.satisfy
  char = liftParsec . P.char
  notChar = liftParsec . P.notChar
  anyChar = liftParsec P.anyChar
  string = liftParsec . P.string

parsecWarning :: PWarnType -> String -> ParsecParser ()
parsecWarning t w = liftParsec $ do
  spos <- Parsec.getPosition
  Parsec.modifyState
    (PWarning t (Position (Parsec.sourceLine spos) (Parsec.sourceColumn spos)) w :)

askCabalSpecVersion :: ParsecParser CabalSpecVersion
askCabalSpecVersion = PP pure

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: Parsec a => String -> Maybe a
simpleParsec =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Like 'simpleParsec' but for 'ByteString'
simpleParsecBS :: Parsec a => ByteString -> Maybe a
simpleParsecBS =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromBS

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
--
-- @since 3.4.0.0
simpleParsec' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsec' spec =
  either (const Nothing) Just
    . runParsecParser' spec lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
-- Fail if there are any warnings.
--
-- @since 3.4.0.0
simpleParsecW' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsecW' spec =
  either (const Nothing) (\(x, ws) -> if null ws then Just x else Nothing)
    . runParsecParser' spec ((,) <$> lexemeParsec <*> liftParsec Parsec.getState) "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: Parsec a => String -> Either String a
eitherParsec = explicitEitherParsec parsec

-- | Parse a 'String' with given 'ParsecParser'. Trailing whitespace is accepted.
explicitEitherParsec :: ParsecParser a -> String -> Either String a
explicitEitherParsec parser =
  either (Left . show) Right
    . runParsecParser (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with given 'ParsecParser' and 'CabalSpecVersion'. Trailing whitespace is accepted.
-- See 'explicitEitherParsec'.
--
-- @since 3.4.0.0
explicitEitherParsec' :: CabalSpecVersion -> ParsecParser a -> String -> Either String a
explicitEitherParsec' spec parser =
  either (Left . show) Right
    . runParsecParser' spec (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Run 'ParsecParser' with 'cabalSpecLatest'.
runParsecParser :: ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser = runParsecParser' cabalSpecLatest

-- | Like 'runParsecParser' but lets specify 'CabalSpecVersion' used.
--
-- @since 3.0.0.0
runParsecParser' :: CabalSpecVersion -> ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser' v p n = Parsec.runParser (unPP p v <* P.eof) [] n

instance Parsec a => Parsec (Identity a) where
  parsec = Identity <$> parsec

instance Parsec Bool where
  parsec = P.munch1 isAlpha >>= postprocess
    where
      postprocess str
        | str == "True" = pure True
        | str == "False" = pure False
        | lstr == "true" = parsecWarning PWTBoolCase caseWarning *> pure True
        | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
        | otherwise = fail $ "Not a boolean: " ++ str
        where
          lstr = map toLower str
          caseWarning =
            "Boolean values are case sensitive, use 'True' or 'False'."

-- | @[^ ,]@
parsecToken :: ParsecParser String
parsecToken = parsecHaskellString <|> ((P.munch1 (\x -> not (isSpace x) && x /= ',') P.<?> "identifier") >>= checkNotDoubleDash)

-- | @[^ ]@
parsecToken' :: ParsecParser String
parsecToken' = parsecHaskellString <|> ((P.munch1 (not . isSpace) P.<?> "token") >>= checkNotDoubleDash)

checkNotDoubleDash :: String -> ParsecParser String
checkNotDoubleDash s = do
  when (s == "--") $
    parsecWarning PWTDoubleDash $
      unwords
        [ "Double-dash token found."
        , "Note: there are no end-of-line comments in .cabal files, only whole line comments."
        , "Use \"--\" (quoted double dash) to silence this warning, if you actually want -- token"
        ]

  return s

parsecFilePath :: ParsecParser FilePath
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
parsecStandard :: Parsec ver => (ver -> String -> a) -> ParsecParser a
parsecStandard f = do
  cs <- some $ P.try (component <* P.char '-')
  ver <- parsec
  let name = map toLower (intercalate "-" cs)
  return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs

-- each component must contain an alphabetic character, to avoid
-- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList :: ParsecParser a -> ParsecParser [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

parsecCommaNonEmpty :: ParsecParser a -> ParsecParser (NonEmpty a)
parsecCommaNonEmpty p = P.sepByNonEmpty (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

-- | Like 'parsecCommaList' but accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- @
parsecLeadingCommaList :: ParsecParser a -> ParsecParser [a]
parsecLeadingCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> toList <$> P.sepEndByNonEmpty lp comma <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

-- |
--
-- @since 3.4.0.0
parsecLeadingCommaNonEmpty :: ParsecParser a -> ParsecParser (NonEmpty a)
parsecLeadingCommaNonEmpty p = do
  c <- P.optional comma
  case c of
    Nothing -> P.sepEndByNonEmpty lp comma
    Just _ -> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

parsecOptCommaList :: ParsecParser a -> ParsecParser [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *> P.spaces

-- | Like 'parsecOptCommaList' but
--
-- * require all or none commas
-- * accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- p*            -- no commas: many p
-- @
--
-- @since 3.0.0.0
parsecLeadingOptCommaList :: ParsecParser a -> ParsecParser [a]
parsecLeadingOptCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> sepEndBy1Start <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

    sepEndBy1Start = do
      x <- lp
      c <- P.optional comma
      case c of
        Nothing -> (x :) <$> many lp
        Just _ -> (x :) <$> P.sepEndBy lp comma

-- | Content isn't unquoted
parsecQuoted :: ParsecParser a -> ParsecParser a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted :: ParsecParser a -> ParsecParser a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecUnqualComponentName :: ParsecParser String
parsecUnqualComponentName = state0 DList.empty
  where
    --
    -- using @kleene@ package we can easily see that
    -- we need only two states to recognize
    -- unqual-component-name
    --
    -- Compare with declarative
    -- 'Distribution.FieldGrammar.Described.reUnqualComponent'.
    --
    -- @
    -- import Kleene
    -- import Kleene.Internal.Pretty
    -- import Algebra.Lattice
    -- import Data.Char
    --
    -- import qualified Data.RangeSet.Map as RSet
    --
    -- main = do
    --     -- this is an approximation, to get an idea.
    --     let component :: RE Char
    --         component = star alphaNum <> alpha <> star alphaNum
    --
    --         alphaNum = alpha \/ num
    --         alpha    = unions $ map char ['a'..'z']
    --         num      = unions $ map char ['0'..'9']
    --
    --         re :: RE Char
    --         re = component <> star (char '-' <> component)
    --
    --     putPretty re
    --     putPretty $ fromTM re
    -- @

    state0 :: DList.DList Char -> ParsecParser String
    state0 acc = do
      c <- ch -- <|> fail ("Invalid component, after " ++ DList.toList acc)
      case () of
        _
          | isDigit c -> state0 (DList.snoc acc c)
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> fail ("Empty component, after " ++ DList.toList acc)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    state1 :: DList.DList Char -> ParsecParser String
    state1 acc = state1' acc `alt` return (DList.toList acc)

    state1' :: DList.DList Char -> ParsecParser String
    state1' acc = do
      c <- ch
      case () of
        _
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> state0 (DList.snoc acc c)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    ch :: ParsecParser Char
    !ch = P.satisfy (\c -> isAlphaNum c || c == '-')

    alt :: ParsecParser String -> ParsecParser String -> ParsecParser String
    !alt = (<|>)

stringLiteral :: ParsecParser String
stringLiteral = lit
  where
    lit :: ParsecParser String
    lit =
      foldr (maybe id (:)) ""
        <$> P.between (P.char '"') (P.char '"' P.<?> "end of string") (many stringChar)
        P.<?> "string"

    stringChar :: ParsecParser (Maybe Char)
    stringChar =
      Just <$> stringLetter
        <|> stringEscape
        P.<?> "string character"

    stringLetter :: ParsecParser Char
    stringLetter = P.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: ParsecParser (Maybe Char)
    stringEscape = P.char '\\' *> esc
      where
        esc :: ParsecParser (Maybe Char)
        esc =
          Nothing <$ escapeGap
            <|> Nothing <$ escapeEmpty
            <|> Just <$> escapeCode

    escapeEmpty, escapeGap :: ParsecParser Char
    escapeEmpty = P.char '&'
    escapeGap = P.skipSpaces1 *> (P.char '\\' P.<?> "end of string gap")

escapeCode :: ParsecParser Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) P.<?> "escape code"
  where
    charControl, charNum :: ParsecParser Char
    charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (P.char '^' *> (P.upper <|> P.char '@'))
    charNum = toEnum <$> num
      where
        num :: ParsecParser Int
        num =
          bounded 10 maxchar
            <|> (P.char 'o' *> bounded 8 maxchar)
            <|> (P.char 'x' *> bounded 16 maxchar)
        maxchar = fromEnum (maxBound :: Char)

    bounded :: Int -> Int -> ParsecParser Int
    bounded base bnd =
      foldl' (\x d -> base * x + digitToInt d) 0
        <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
      where
        thedigits :: [ParsecParser Char]
        thedigits = map P.char ['0' .. '9'] ++ map P.oneOf (transpose [['A' .. 'F'], ['a' .. 'f']])

        toomuch :: ParsecParser a
        toomuch = P.unexpected "out-of-range numeric escape sequence"

        bounded', bounded'' :: [ParsecParser Char] -> [Int] -> ParsecParser [Char]
        bounded' dps@(zero : _) bds =
          P.skipSome zero *> ([] <$ P.notFollowedBy (P.choice dps) <|> bounded'' dps bds)
            <|> bounded'' dps bds
        bounded' [] _ = error "bounded called with base 0"
        bounded'' dps [] = [] <$ P.notFollowedBy (P.choice dps) <|> toomuch
        bounded'' dps (bd : bds) =
          let anyd :: ParsecParser Char
              anyd = P.choice dps

              nomore :: ParsecParser ()
              nomore = P.notFollowedBy anyd <|> toomuch

              (low, ex, high) = case splitAt bd dps of
                (low', ex' : high') -> (low', ex', high')
                (_, _) -> error "escapeCode: Logic error"
           in ((:) <$> P.choice low <*> atMost (length bds) anyd) <* nomore
                <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                <|> if not (null bds)
                  then (:) <$> P.choice high <*> atMost (length bds - 1) anyd <* nomore
                  else empty
        atMost n p
          | n <= 0 = pure []
          | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

    charEsc :: ParsecParser Char
    charEsc = P.choice $ parseEsc <$> escMap

    parseEsc (c, code) = code <$ P.char c
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

    charAscii :: ParsecParser Char
    charAscii = P.choice $ parseAscii <$> asciiMap

    parseAscii (asc, code) = P.try $ code <$ P.string asc
    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
    ascii2codes, ascii3codes :: [String]
    ascii2codes =
      [ "BS"
      , "HT"
      , "LF"
      , "VT"
      , "FF"
      , "CR"
      , "SO"
      , "SI"
      , "EM"
      , "FS"
      , "GS"
      , "RS"
      , "US"
      , "SP"
      ]
    ascii3codes =
      [ "NUL"
      , "SOH"
      , "STX"
      , "ETX"
      , "EOT"
      , "ENQ"
      , "ACK"
      , "BEL"
      , "DLE"
      , "DC1"
      , "DC2"
      , "DC3"
      , "DC4"
      , "NAK"
      , "SYN"
      , "ETB"
      , "CAN"
      , "SUB"
      , "ESC"
      , "DEL"
      ]
    ascii2, ascii3 :: String
    ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
    ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"
