module Generation.X86_64.Register
    ( argRegName64
    )
  where

import Data.Array


registersGP = [ "rax"
              , "rcx"
              , "rdx"
              , "rsi"
              , "rdi"
              , "rsp"
              , "rbp"
              , "r8"
              , "r9"
              , "r10"
              , "r11"
              , "r12"
              , "r13"
              , "r14"
              , "r15"
              ]

argRegs64 = listArray (0,5) ["rsi", "rdi", "rdx", "rcx", "r8", "r9"]

argRegName64 :: Int -> String
argRegName64 i = argRegs64 ! i
