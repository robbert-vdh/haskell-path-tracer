-- | Template Haskell macros for importing shaders.
module TH where

import Language.Haskell.TH

-- | Read a shader file at compile time as a string.
readShaderQ :: FilePath -> Q Exp
readShaderQ fp = LitE . StringL <$> runIO (readFile fp)
