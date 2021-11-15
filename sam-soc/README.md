# Getting Started
SAMSoc works in Linux environment. To use SAMSoc, the GHC and cabal are required. SAMSoc can be installed by using Cabal. After installing SAMSoc through Cabal, an example of using SAMSoc is presented as below.

## Build
```bash
cabal install
```

## Usage
```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Distributed.SAMSoc 

input = [1 .. 100] :: [Int]

incr :: Int -> Int
incr x = x + 1

afterFunc :: [Int] -> IO ()
afterFunc xs = putStrLn $ show $ sum xs

slaveJob :: ProcessId -> Process()
slaveJob = $(mkSlave 'incr)

remotable ['slaveJob]

main = do
    ret <- $(mCoreMap 'slaveJob) 80 3 input
    afterFunc ret

```

