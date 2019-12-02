import Data.Bits

type Registers = [Int]

--data Instruction = Instruction (Int -> Int -> Int)
type Instruction = Registers -> Int -> Int -> Int -> Registers

setReg :: Registers -> Int -> Int -> Registers
setReg (curreg:remreg) c val
    | c == 0    = val : setReg remreg (c-1) val
    | otherwise = curreg : setReg remreg (c-1) val
setReg [] _ _ = []

processRegReg :: (Int -> Int -> Int) -> Registers -> Int -> Int -> Int -> Registers
processRegReg inst inreg a b c = setReg inreg c $ inst rega regb
    where
        rega = inreg !! a
        regb = inreg !! b

processRegImm :: (Int -> Int -> Int) -> Registers -> Int -> Int -> Int -> Registers
processRegImm inst inreg a b c = setReg inreg c $ inst rega b
    where
        rega = inreg !! a

processImmReg :: (Int -> Int -> Int) -> Registers -> Int -> Int -> Int -> Registers
processImmReg inst inreg a b c = setReg inreg c $ inst a regb
    where
        regb = inreg !! b

addinst :: Int -> Int -> Int
addinst a b = a + b

mulinst :: Int -> Int -> Int
mulinst a b = a * b

borinst :: Int -> Int -> Int
borinst a b = a .|. b

baninst :: Int -> Int -> Int
baninst a b = a .&. b

setinst :: Int -> Int -> Int
setinst a _ = a

gtinst :: Int -> Int -> Int
gtinst a b = if a > b then 1 else 0

eqinst :: Int -> Int -> Int
eqinst a b = if a == b then 1 else 0

addr :: Instruction
addr = processRegReg addinst
addi :: Instruction
addi = processRegImm addinst
mulr :: Instruction
mulr = processRegReg mulinst
muli :: Instruction
muli = processRegImm mulinst
borr :: Instruction
borr = processRegReg borinst
bori :: Instruction
bori = processRegImm borinst
banr :: Instruction
banr = processRegReg baninst
bani :: Instruction
bani = processRegImm baninst
setr :: Instruction
setr = processRegReg setinst
seti :: Instruction
seti = processImmReg setinst
gtir :: Instruction
gtir = processImmReg gtinst
gtri :: Instruction
gtri = processRegImm gtinst
gtrr :: Instruction
gtrr = processRegReg gtinst
eqir :: Instruction
eqir = processImmReg eqinst
eqri :: Instruction
eqri = processRegImm eqinst
eqrr :: Instruction
eqrr = processRegReg eqinst

main :: IO ()
main = do
    let stregs = [3,2,1,1]
        fnregs = [3,2,2,1]
        cmd = [9,2,1,2]
        allinsts = [addr, addi, mulr, muli, borr, bori, banr, bani, setr, seti,
            gtir, gtri, gtrr, eqir, eqri, eqrr]
        results = [inst stregs (cmd !! 1) (cmd !! 2) (cmd !! 3) | inst <- allinsts]
        correct = [fnregs == resreg | resreg <- results]
    putStrLn $ show correct
