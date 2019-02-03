module Lib where

import Language.C.Clang
import Control.Monad.Trans.State.Lazy

--State

data MyState = MyState { sNameSpace :: String
                       , sPath :: String
                       , sName :: String
                       , sCtor :: Int
                       }
                       deriving Show


setNameSpace :: String -> State MyState ()
setNameSpace ns = modify (\s -> s {sNameSpace = ns})

setCtor :: Int -> State MyState ()
setCtor i = modify (\s -> s {sCtor = i})

--Utils

myshow :: Show a => [a] -> String
myshow ls = "    [" ++ (drop 5 $ concatMap (\e -> "    ," ++ show e ++ "\n") ls ++ "]")


--Prediates

isFromMain :: Cursor -> Bool
isFromMain c = let ismain = isFromMainFile.rangeStart <$> cursorExtent c
               in case ismain of
                  Just True -> True
                  _         -> False

notLastExtraDecl :: Cursor -> Bool
notLastExtraDecl c = case cursorKind c of
                      LastExtraDecl -> False
                      _ -> True


notCompound :: Cursor -> Bool
notCompound c = case cursorKind c of
                  CompoundStmt -> False
                  _ -> True

notNSRef :: Cursor -> Bool
notNSRef c = case cursorKind c of
                  NamespaceRef -> False
                  _ -> True


isParam :: Cursor -> Bool
isParam c = case cursorKind c of
                  ParmDecl -> True
                  _ -> False


