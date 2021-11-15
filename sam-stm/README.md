# Getting Started
SAMSTM works in Linux environment. To use SAMSTM, the GHC and cabal are required. SAMSTM can be installed by using Cabal. After installing SAMSTM through Cabal, an example of using SAMSTM is presented as below.

## Build
```bash
cabal install
```

## Usage
```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Distributed.SAMSTM 

input = [1 .. 100] :: [Int]

incr :: Int -> Int
incr x = x + 1

afterFunc :: [Int] -> IO ()
afterFunc xs = putStrLn $ show $ sum xs

slaveJob :: ProcessId -> Process()
slaveJob = $(mkSlave 'incr)

remotable ['slaveJob]

main = do
    transport <- createTransport
    mSlavesNodeId <- $runSlaveInMemory nthread transport
    slavesNodeIdList <- mapM (\i -> takeMVar i) mSlavesNodeId

    ret <- $(mCoreMapInMemory 'slaveJob) input slavesNodeIdList 
    afterFunc ret

```

