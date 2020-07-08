{-# LANGUAGE TemplateHaskell #-}

-- | Inlined versions of various files to reduce splicing TH in the main module.
module Files where

import qualified Data.ByteString.Char8         as BS
import           TH

-- | The font used for showing the number of iterations.
font :: BS.ByteString
font = $(readFileBsQ "app/assets/OpenSans-Regular.ttf")

-- | Shaders used for drawing the screen quad. We devide the accumulated pixel
-- values by the number of iterations inside of the shader, and then we blit the
-- text rendered by @sdl2_ttf@ .
vertexShader, fragmentShader :: BS.ByteString
vertexShader = $(readFileBsQ "app/assets/vs.glsl")
fragmentShader = $(readFileBsQ "app/assets/fs.glsl")
