{
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-tabs               #-}
{-# OPTIONS_GHC -fno-warn-unused-binds       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}

module Language.Sexp.Lexer
  ( lexSexp
  ) where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B8

import Language.Sexp.LexerInterface
import Language.Sexp.Token
import Language.Sexp.Types (Position (..))

}

$whitechar   = [\ \t\n\r\f\v]
$unispace    = \x01
$whitespace  = [$whitechar $unispace]

$uninonspace = \x02
$uniany      = [$unispace $uninonspace]
@any         = (. | $uniany)

$digit       = 0-9
$hex         = [0-9 A-F a-f]
$alpha       = [a-z A-Z]

$graphic     = [$alpha $digit \!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~ \(\)\,\;\[\]\`\{\} \:\"\'\_ $uninonspace]

@intnum      = [\-\+]? $digit+
@scinum      = [\-\+]? $digit+ ([\.]$digit+)? ([eE] [\-\+]? $digit+)?

$charesc     = [abfnrtv\\\"]
@escape      = \\ ($charesc | $digit+ | x $hex+)
@string      = $graphic # [\"\\] | " " | @escape

$idinitial   = [$alpha \@\!\$\%\&\*\/\<\=\>\?\~\_\^\.\+\- $uninonspace]
$idsubseq    = [$idinitial $digit \: $uninonspace]
@identifier  = $idinitial $idsubseq*
@keyword     = ":" $idsubseq+

:-

$whitespace+       ;
";" @any*          ;
"("                { just TokLParen       }
")"                { just TokRParen       }
"["                { just TokLBracket     }
"]"                { just TokRBracket     }
"'" / $graphic     { just TokQuote        }
"#t"               { just (TokBool True)  }
"#f"               { just (TokBool False) }
"#" / $graphic     { just TokHash         }
@intnum            { TokInt     `via` readInteger       }
@scinum            { TokReal    `via` (read . T.unpack) }
@identifier        { TokSymbol  `via` id                }
@keyword           { TokKeyword `via` id                }
\" @string* \"     { TokStr     `via` readString        }
.                  { TokUnknown `via` T.head            }

{

type AlexAction = LineCol -> TL.Text -> LocatedBy LineCol Token

readInteger :: T.Text -> Integer
readInteger str =
  case signed decimal str of
    Left err -> error $ "Lexer is broken: " ++ err
    Right (a, rest)
      | T.null (T.strip rest) -> a
      | otherwise -> error $ "Lexer is broken, leftover: " ++ show rest

readString :: T.Text -> T.Text
readString =
  T.pack . read . T.unpack

just :: Token -> AlexAction
just tok pos _ =
  L pos tok

via :: (a -> Token) -> (T.Text -> a) -> AlexAction
via ftok f pos str =
  L pos . ftok . f . TL.toStrict $str

alexScanTokens :: AlexInput -> [LocatedBy LineCol Token]
alexScanTokens input =
  case alexScan input defaultCode of
    AlexEOF -> []
    AlexError (AlexInput {aiInput, aiLineCol = LineCol line col}) ->
      error $ "Lexical error at line " ++ show line ++ " column " ++ show col ++
        ". Remaining input: " ++ TL.unpack (TL.take 1000 aiInput)
    AlexSkip input _ -> alexScanTokens input
    AlexToken input' tokLen action ->
      action (aiLineCol input) inputText : alexScanTokens input'
      where
        -- It is safe to take token length from input because every byte Alex
        -- sees corresponds to exactly one character, even if character is a
        -- Unicode one that occupies several bytes. We do character translation
        -- in LexerInterface.alexGetByte function so that all unicode characters
        -- occupy single byte.
        --
        -- On the other hand, taking N characters from Text will take N valid
        -- characters, not N bytes.
        --
        -- Thus, we're good.
        inputText = TL.take (fromIntegral tokLen) $ aiInput input
  where
    defaultCode :: Int
    defaultCode = 0

lexSexp :: Position -> TL.Text -> [LocatedBy Position Token]
lexSexp (Position fn line1 col1) =
  map (mapPosition fixPos) . alexScanTokens . mkAlexInput
  where
    fixPos (LineCol l c) | l == 1    = Position fn line1 (col1 + c)
                         | otherwise = Position fn (pred l + line1) c

}
