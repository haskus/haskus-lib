module Main (main) where

import Haskus.Arch.X86.Assembler

main :: IO ()
main = do
  putStrLn "Test assembler."

  runAsm $ assembleList
    [ Insn [] AAA []
    , Insn [] AAD []
    , Insn [] AAD [10]
    , Insn [] AAM []
    , Insn [] AAM [10]
    ]
