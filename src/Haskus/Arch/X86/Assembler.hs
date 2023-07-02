{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskus.Arch.X86.Assembler where

import Data.Word
import Data.Int
import Numeric


data Opcode
  = AAA
  | AAD
  | AAM
  | AAS
  | ADC
  deriving (Show,Eq,Ord)

data Reg
  = AL | AX | EAX | RAX | AH
  | BL | BX | EBX | RBX | BH
  | CL | CX | ECX | RCX | CH
  | DL | DX | EDX | RDX | DH
  | BP | EBP | RBP
  | SP | ESP | RSP
  deriving (Show,Eq,Ord)

-- | TODO: sublanguage for memory addresses with labels/expressions
data Expr
  = ELabel String
  deriving (Show)

data Mem
  = MemAddr !Word64
  | MemExpr !Expr
  | MemSIB  !Reg !Int8 !Reg
  deriving (Show)

data Imm
  = Imm8u   !Word8
  | Imm16u  !Word16
  | Imm32u  !Word32
  | Imm64u  !Word64
  | Imm8s   !Int8
  | Imm16s  !Int16
  | Imm32s  !Int32
  | Imm64s  !Int64
  | ImmX    !Integer
  deriving (Eq,Ord,Show)

data Operand
  = OpReg !Reg
  | OpMem !Mem
  | OpImm !Imm
  deriving (Show)

-- just to support OverloadedLiterals
instance Num Operand where
  fromInteger x = OpImm (ImmX x)
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined

data InsnOpt
  = Lock
  deriving (Show,Eq,Ord)

type InsnOpts = [InsnOpt]

data InsnFlag
  = Arch8086
  | NoLong
  | Long
  | CanLock
  deriving (Show,Eq,Ord)

type InsnFlags = [InsnFlag]


data Insn = Insn InsnOpts Opcode [Operand]

newtype Asm a
  = Asm (IO a)
  deriving newtype (Functor,Applicative,Monad)

runAsm :: Asm a -> IO a
runAsm (Asm m) = m

emit :: Word8 -> Asm ()
emit x = Asm $ putStr (showHex x "")

-- | Check that the instruction flags are valid for the current context.
check :: InsnOpts -> InsnFlags -> Asm ()
check opts flags = if
  | Lock `elem` opts && CanLock `notElem` flags
  -> error "Can't lock this"
  | otherwise
  -> return ()

assembleList :: [Insn] -> Asm ()
assembleList is = mapM_ assembleInsn is

assembleInsn :: Insn -> Asm ()
assembleInsn (Insn opts oc ops) = assemble opts oc ops

expectImm8u :: Operand -> Asm Word8
expectImm8u = \case
  OpImm (Imm8u x)
    -> pure x
  OpImm (ImmX x)
    | x >= 0 && x <= 0xFF
    -> pure (fromIntegral x)
  i -> error $ "Invalid immediate operand. Expected unsigned 8-bit immediate, got " ++ show i


-- | Assemble an instruction
assemble :: InsnOpts -> Opcode -> [Operand] -> Asm ()
assemble opts = \cases
  AAA [] -> do
    check opts [Arch8086,NoLong]
    emit 0x37
  
  AAD [] -> do
    check opts [Arch8086,NoLong]
    emit 0xd5
    emit 0x0a
  
  AAD [x] -> do
    check opts [Arch8086,NoLong]
    i <- expectImm8u x
    emit 0xd5
    emit i

  AAM [] -> do
    check opts [Arch8086,NoLong]
    emit 0x45
    emit 0x0a
  
  AAM [x] -> do
    check opts [Arch8086,NoLong]
    i <- expectImm8u x
    emit 0x45
    emit i

  AAS [] -> do
    check opts [Arch8086,NoLong]
    emit 0x3f

  oc ops -> do
    error $ "Unexpected instruction: " ++ show oc ++ " " ++ show ops
