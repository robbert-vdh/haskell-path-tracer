{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell macros for importing shaders.
module TH where

import qualified Data.ByteString.Char8 as BS
import Language.Haskell.TH
import System.IO

-- | Read a file at compile time as a 'ByteString'. 'readFile' can't read our
-- .tff font since it contains bytes that would be invalid in UTF-8. The
-- 'latin1' encoding will cause errors to be thrown when encountering codepoints
-- above the first 256 unicode characters, but this is probably for the best.
readFileBsQ :: FilePath -> Q Exp
readFileBsQ fp = [|BS.pack $(LitE . StringL <$> runIO (readBytes fp))|]
  where
    readBytes f = do
      handle <- openFile f ReadMode
      hSetEncoding handle latin1
      hGetContents handle
