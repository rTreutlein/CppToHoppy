module Main where

import Prelude hiding ((.))

import Lib

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Category
import Language.C.Clang

import Data.List.Split
import qualified Data.ByteString.Char8 as BS

import Debug.Trace

main :: IO ()
main = do
    putStrLn "Please Provide the Root Directory of the Project."
    root <- getLine
    putStrLn "Please Provide the relative Sub-Directory to Process."
    rel_path <- getLine
    let full_path = root ++ rel_path
    all_elems <- listDirectory full_path
    let elems = filter (\e -> takeExtension e == ".h") all_elems
        full_path_elems = map ((full_path ++ "/") ++) elems

    results <- mapM (parseFile rel_path) $ zip full_path_elems elems

    forM_ results putStrLn

parseFile :: FilePath -> (FilePath,FilePath) -> IO (String)
parseFile rel_path (path,realName) = do
    idx <- createIndex
    tu <- parseTranslationUnit idx path ["-xc++-header"
                                        ,"-I/usr/lib/clang/7.0.1/include"]
    putStrLn path
    let cs = filter isFromMain $ cursorChildren $ translationUnitCursor tu
        state = MyState {sNameSpace = "" , sPath = rel_path
                        ,sName = realName, sCtor = 0}
        r = concatMap (\c -> evalState (c_toHoppy c) state) cs
        rs = map (\c -> if c=='\\' then '"' else c) r
    pure rs

c_toHoppy :: Cursor -> State MyState String
c_toHoppy c =
  case cursorKind c of
      Namespace ->
        let cs = cursorChildren c
        in do
            setNameSpace $ BS.unpack $ cursorSpelling c
            top_level_defs <- mapM c_toHoppy cs
            setNameSpace ""
            pure $ concatMap (\s -> s ++ "\n") top_level_defs

      ClassDecl ->
        --LastExtraDecl are Friend declarations we don't care about those
        let cs = filter notLastExtraDecl $ cursorChildren c
            name = BS.unpack $ cursorSpelling c
            cname = "c_" ++ name
            tdef = cname ++ " :: Class\n" ++
                   cname ++ " =\n"
            (base,cs2) = handleBases cs -- $ trace (myshow cs) cs
        in do
            class_elems <- mapM c_toHoppy $ (onlyPublic cs2)
            dir_path <- gets sPath
            file_name <- gets sName
            ident <- getIdent name
            let file_path = dir_path ++ file_name
                req  = "    addReqIncludes [includeStd \"" ++ file_path ++ "\"] $\n"
                mc   = "    makeClass " ++ ident ++ " Nothing\n"
                methods = filter ((/=) '"') $ myshow class_elems
            pure $ tdef ++ req ++ mc ++ "    " ++ base ++ "\n" ++ methods

      FunctionDecl -> do
        let (x:xs) = cursorChildren c
            name = BS.unpack $ cursorSpelling c
            op = checkOperator name
        (ps,r) <- case op of
            Just s -> do
                paramTs <- mapM c_toHoppy $ filter notCompound (x:xs)
                pure (paramTs,s)
            Nothing -> do
                paramTs <- mapM c_toHoppy $ filter notCompound xs
                retT <- c_toHoppy x
                pure (paramTs,retT)

        ident <- getIdent name
        let params = filter ((/=) '"') $ show ps

        --pure $ "makeFn " ++ ident ++ " Nothing Nonpure " ++ params ++ r
        pure $ maybe "" t_toHoppy (cursorType c)

      CXXMethod -> do
          let cs = filter notNSRef $ cursorChildren c
              name = BS.unpack $ cursorSpelling c
              op = checkOperator name
              mk = if isMethodConst c
                      then "mkConstMethod "
                      else if isMethodStatic c
                              then "mkStaticMethod "
                              else "mkMethod "
          (ps,r) <- case op of
              Just s -> do
                  paramTs <- mapM c_toHoppy $ filter notCompound cs
                  pure (paramTs,s)
              Nothing ->
                case cs of
                    [] -> pure ([],"")
                    (x:xs) -> do
                        paramTs <- mapM c_toHoppy $ filter notCompound xs
                        retT <- c_toHoppy x
                        pure (paramTs,retT)

          ident <- getIdent name
          let params = filter ((/=) '"') $ show ps

          --pure $ mk ++ ident ++ " Nothing Nonpure " ++ params ++ [] ++ r
          pure $ maybe "" t_toHoppy (cursorType c)

      TypeRef ->
        case cursorType c of
            Nothing -> pure $ "ParmDecl had no Type"
            Just t  -> pure $ t_toHoppy t
      ParmDecl ->
        case cursorType c of
            Nothing -> pure $ "ParmDecl had no Type"
            Just t  -> pure $ t_toHoppy t
      CompoundStmt -> pure $ show c ++ show (cursorType c)
      TemplateRef -> pure $ BS.unpack $ cursorSpelling c
      Constructor -> do
          let cs = filter isParam $ cursorChildren c
          params <- mapM c_toHoppy cs
          let paramsS = filter ((/=) '"') $ show params
          ctor_count <- gets sCtor
          setCtor (ctor_count + 1)
          pure $ "mkCtor \"new" ++ show ctor_count ++ "\" " ++ paramsS
      LastExtraDecl -> pure "If this is a friend should be ignored."
      k -> pure $ show k

--  [ mkCtor "new" [doubleT]
--  , mkConstMethod "value" [] $ objT $ c_vector dvec

checkOperator :: String -> Maybe String
checkOperator "operator==" = Just "boolT"
checkOperator "operator!=" = Just "boolT"
checkOperator "operator<=" = Just "boolT"
checkOperator "operator>=" = Just "boolT"
checkOperator "operator<" = Just "boolT"
checkOperator "operator>" = Just "boolT"
checkOperator _ = Nothing

handleBases :: [Cursor] -> (String,[Cursor])
handleBases (c:cs) = case cursorKind c of
    CXXBaseSpecifier ->
        let base = BS.unpack $ cursorSpelling c
            name = 'c' : '_' : (last $ splitOn "::" base)
        in ("[" ++ name ++ "]",cs)
    _ -> ("[]",c:cs)

onlyPublic :: [Cursor] -> [Cursor]
onlyPublic [] = []
onlyPublic (c:cs) = case cursorKind c of
    CXXAccessSpecifier -> onlyPublic cs
    _ -> if cursorAccess c == Just Public
            then c : onlyPublic cs
            else onlyPublic cs

getIdent :: String -> State MyState String
getIdent name = do
    namespace <- gets sNameSpace
    let ident = if namespace /= ""
                  then "(idnet1 \"" ++ namespace ++ "\" \"" ++ name ++ "\")"
                  else "(idnet \"" ++ name ++ "\")"
    pure ident

t_toHoppy :: Type -> String
t_toHoppy t =
  case typeKind t of
      LValueReference ->
          let ts1 = BS.unpack $ typeSpelling t
              (cb,ts2) = hasConst ts1
              (pt,ts3) = isRefPtr ts2
              n = 't' : '_' : (last $ splitOn "::" ts3)
          in '(' : pt ++ cb ++ "objT) " ++ n
      Typedef ->
          let ts1 = BS.unpack $ typeSpelling t
              (cb,ts2) = hasConst ts1
              (pt,ts3) = isRefPtr ts2
              n = 't' : '_' : (last $ splitOn "::" ts3)
          in '(' : pt ++ cb ++ "objT) " ++ n
      Bool -> "boolT"
      Int -> "intT"
      Double -> "doubleT"
      FunctionProto ->
        let args = typeArgs t
            retT = typeResult t
        in show args ++ show retT
      k -> show k ++ "No Handeling"

hasConst :: String -> (String,String)
hasConst s =
  case splitAt 6 s of
    ("const ",r) -> ("constT.",r)
    _ -> ("",s)

isRefPtr :: String -> (String,String)
isRefPtr s =
  case splitAt (length s - 2) s of
    (r," *") -> ("ptrT.",r)
    (r," &") -> ("refT.",r)
    _ -> ("",s)

