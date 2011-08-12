{-# LANGUAGE DeriveDataTypeable #-}
module Zoom.Args where
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
arguments :: Mode [(String,String)]
arguments = mode "explicit" [] "Explicit sample program" (flagArg (upd "file") "FILE") [flagOpt "world" ["hello","h"] (upd "world") "WHO" "World argument"
     ,flagReq ["greeting","g"] (upd "greeting") "MSG" "Greeting to give"
     ,flagHelpSimple (("help",""):)]
  where upd msg x v = Right $ (msg,x):v

main = do
  xs <- processArgs arguments
  if ("help","") `elem` xs then
    print $ helpText def arguments
  else
    print xs