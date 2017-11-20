# SAM
Simple actor model for manycore (SAM) is a parallel programming model for manycore environments. This repository contains the parallel programming package for the Haskell language.

SAM is licensed under the BSD3 license. The detail of the licience can be found in the LICIENCE file.

# Introduction
Haskell is easy for beginners to write parallel programs by using the parallel package. E.g., you can implement parallelism on "map" function by only changing it to "parMap". However, since there are issues such as GC it is difficult to fully expand the performance of parallelism in manycore environment. On the other hand, Cloud Haskell can present satisfied scalability in the manycore environment. But since Cloud Haskell requires a detailed "Master/Slave" configuration which is difficult for beginners. This project provides a package named SAM that supports both coding efficiency of Haskell's parallel package and scalability of Cloud Haskell.

SAM provides a scalability and coding efficiency on the manycore environment. This package guarantees a scalability that using the Cloud Haskell on local node. And using this package, you can do map-style programming in parallel program development. Since using SAM can easily write parallel programs in manycore environment, it is not only suitable for beginners of parallel programming but also appropriate for programmers who develop applications in manycore environment.

# Getting Started
SAM works in Linux environment. To use SAM, the GHC and cabal are required.

This section will be released after the paper is published.

# Contact Information
Please send e-mail: pllab@pusan.ac.kr
