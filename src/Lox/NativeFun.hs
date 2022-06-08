module Lox.NativeFun where
-- we've got bigger fish to fry than telling time
{-
import Control.Monad ((=<<),  (<=<))
import Lox.Types
import Lox.Interp
import Lox.Helpers
import Data.Time.Clock.System
import Polysemy.Final (embedFinal)
import Data.Map qualified as M
import Data.Text qualified as T
import Polysemy
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
clock :: LoxValue
clock = makeNativeFun "clock" (\_ -> LoxNumber . fromIntegral . systemSeconds <$>  embedFinal getSystemTime) 0
{-# NOINLINE clock #-}
makeNativeFunction :: Members LxMembers r => T.Text -> ([LoxValue] -> Sem r LoxValue) -> Int -> LoxValue
makeNativeFunction name fun arity = LoxFun name arity [\args -> execute . LReturn . Just . Expr Literal <$> fun args] M.empty 

nativeFuns = [clock]
envWithNative :: (Stack, Heap)
envWithNative = 
  let stack' = M.fromList (map (\f@(LoxFun name _ _ _) -> (name, f)) nativeFuns) in
    undefined

counter :: IORef Int
counter = unsafePerformIO $ newIORef (-1)
{-# NOINLINE counter #-}
getCount :: Int 
getCount = unsafePerformIO $ readIORef counter <* modifyIORef' counter (subtract 1)
{-# NOINLINE getCount #-}
-}
