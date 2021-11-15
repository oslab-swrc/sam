{-
SPDX-FileCopyrightText: Copyright (c) 2021 Pusan National University

SPDX-License-Identifier: BSD-3 License
-}
{-# LANGUAGE TemplateHaskell, BangPatterns, MonoLocalBinds #-}

module Control.Distributed.SAMSTM
(
    mCoreMapInMemory
 ,  mkSlave
 ,  ProcessId
 ,  Process
 ,  remotable
 ,  runSlaveInMemory
) where

import System.Environment
import Data.List
import System.IO
import System.Argv0
import System.Process
import System.Exit (ExitCode, die)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Filesystem.Path.CurrentOS
import Control.Concurrent
import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Primitives (finally)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
--import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process as Ps

import System.Posix.Process as Px

import Control.Concurrent.MVar
import qualified Control.Monad.Catch as CA
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Distributed.SAM.NewSimpleLocalnet

import Network.Transport.InMemory
import qualified Network.Transport as NT
import Control.Distributed.Process.Internal.Types

mkSlave fName = [| \them -> forever $ do
                          (i, n) <- expect
                          let ret = $(varE fName) n
                          send them (i :: Int ,ret) |]

createSlaveNode :: NT.Transport -> RemoteTable -> IO (NodeId,Node.LocalNode)
createSlaveNode transport remote = do
    trans <- Network.Transport.InMemory.createTransport
    node <- Node.newLocalNode trans remote
    return $ ((Node.localNodeId node), node)


runSlaveInMemory = [| \nThread -> \transport ->
    let threads = [1..nThread]
    in do 
        nodeids <- mapM (\n -> newEmptyMVar) threads
        slaves <- forM nodeids $ \nodeid -> do
            forkIO $ do
                (id,ln) <- createSlaveNode transport ($(varE (mkName "__remoteTable")) initRemoteTable)
                putMVar nodeid id
        return nodeids
        |]

mCoreMapInMemory name = [| \src -> \nodeid -> do
    nThread <- getNumCapabilities 
    transport <- createTransport
    node <- Node.newLocalNode transport ($(varE (mkName "__remoteTable")) initRemoteTable)
    ret <- mcoreStartMasterInMemory node nodeid $ \slaves mResult -> do
        us <- getSelfPid
        slaveProcesses <- forM slaves $ \nid -> spawn nid ($(mkClosure name) us)
        let msg = (zip src (cycle slaveProcesses)) 
        let msg_with_order = zip ([1..] :: [Int] ) msg
        spawnLocal $ forM_ msg_with_order $ \ (i,(m, them)) -> send them (i,m)

        let src_size = length src
        ret_with_order <- mergeList src_size
        let ret = map snd ret_with_order
        liftIO $ putMVar mResult ret
    return ret 
    |]

mcoreStartMasterInMemory :: Serializable a => Node.LocalNode -> [NodeId] -> ([NodeId] -> MVar a -> Process()) -> IO (a)
mcoreStartMasterInMemory node slaves proc = do
    result <- newEmptyMVar
    Node.runProcess node $ do
        proc slaves result 
    ret <- takeMVar result
    return ret

mergeList :: Serializable a => Int -> Process [(Int, a)]
mergeList = go []
    where
    go :: Serializable a => [(Int, a)] -> Int -> Process [(Int, a)]
    go !acc 0 = return acc
    go !acc n = do
        (i,m) <- expect 
        go (insertBy (\(a,_) (b,_) -> compare a b) (i,m) acc ) (n-1)


