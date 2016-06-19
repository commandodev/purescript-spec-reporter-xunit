module Data.XML.PrettyPrint (
  Indent(),
  print
  ) where

import Prelude

import Control.Monad.State.Trans (StateT, runStateT, get, modify)
import Control.Monad.Writer (Writer, runWriter, lift)
import Control.Monad.Writer.Class (tell)

import Data.Char                  (fromCharCode)
import Data.String                (fromCharArray, split)
import Data.String.Regex          (regex, replace')
import Data.Tuple                 (Tuple, snd)
import Data.Foldable              (foldl, sequence_)
import Data.Unfoldable            (replicate)

import Data.XML (Attr(..), Document(..), Node(..))

type Indent = Int
type CurrentIndent = Int

-- | The number of spaces in an indent and the current number of indents made.
-- | `PrinterState 2 6` represents 12 spaces.
data PrinterState = PrinterState Indent CurrentIndent

type Printer v = StateT PrinterState (Writer String) v

indent :: Printer Unit
indent = modify $ \(PrinterState i ci) -> PrinterState i (ci + 1)

dedent :: Printer Unit
dedent = modify $ \(PrinterState i ci) -> PrinterState i (ci - 1)

indentSpaces :: Printer String
indentSpaces = do
  (PrinterState indent' currentIndent) <- get
  pure $ fromCharArray $ replicate (indent' * currentIndent) (fromCharCode 32)

appendLine :: String -> Printer Unit
appendLine "" = lift $ tell "\n"
appendLine s = do
  spaces <- indentSpaces
  lift $ tell $ spaces <> s <> "\n"

enclosed :: String -> String -> String -> String
enclosed before after contents = before <> contents <> after

openTag :: String -> Array Attr -> String
openTag contents attrs = enclosed "<" ">" (contents <> showAttrs attrs)

closeTag :: String -> String
closeTag contents = enclosed "</" ">" contents

escape :: String -> String
escape s = replace' (regex "[<>\t\n\r\"]" flags) replacer s
  where replacer "<" _ = "&lt;"
        replacer ">" _ = "&gt;"
        replacer "\"" _ = "&quot;"
        replacer "\t" _ = ""
        replacer "\r" _ = ""
        replacer s _ = s
        flags = {
          unicode: false,
          sticky: false,
          multiline: false,
          ignoreCase: false,
          global: true
        }

printNode :: Node -> Printer Unit
printNode (Comment s) = appendLine $ enclosed "<!-- " " -->" s
printNode (Text s) = do
 sequence_ $ map appendLine $ split "\n" $ escape s
printNode (Element tagName attrs []) = do
  appendLine $ openTag tagName attrs <> closeTag tagName
printNode (Element tagName attrs nodes) = do
  appendLine $ openTag tagName attrs
  indent
  sequence_ $ map printNode nodes
  dedent
  appendLine $ closeTag tagName

showAttrs :: Array Attr -> String
showAttrs as = foldl iter "" as
  where iter acc (Attr key value) = acc <> " " <> key <> "=\"" <> (escape value) <> "\""

printDocument :: Document -> Printer Unit
printDocument (Document version encoding node) = do
  appendLine $ "<?xml" <> (showAttrs [Attr "version" version, Attr "encoding" encoding]) <> "?>"
  printNode node

print :: Indent -> Document -> String
print indent' doc =
  let w = runStateT (printDocument doc) (PrinterState indent' 0)
      wt = (runWriter w) :: Tuple (Tuple Unit PrinterState) String
  in snd wt
