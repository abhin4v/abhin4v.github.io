---
title: Introduction to Concurrency in Haskell
event: Functional Conf 2015, Bengaluru
youtube_id: uVK3t-5wWew
speakerdeck_id: bcbf9a544894420699a16821e5a59074
speakerdeck_ratio: 1.37081659973226
---
Haskell is widely known as a functional programming language which is great to write pure (non I/O) code and immutable data structures. But it is not well known that it has a great support for a wide variety of concurrency models too.

In this talk we explore the various concurrency models and primitives supported by Haskell including green threads, mutable variables, channels and software transactional memory. We see how the purity, laziness, and the strong type system of Haskell come together, and help write clean and modular concurrent code. We explore these by writing a mutli-user chat server with support for private and channel chatting.