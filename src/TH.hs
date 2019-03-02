{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell macros for importing shaders.
module TH where

import qualified Data.ByteString.Char8 as BS
import Language.Haskell.TH

-- | Read a shader file at compile time as a string.
readShaderQ :: FilePath -> Q Exp
readShaderQ fp = [|BS.pack $(LitE . StringL <$> runIO (readFile fp))|]
