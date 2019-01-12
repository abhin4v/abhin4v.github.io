---
title: Many Ways to Concur
event: Functional Conf 2018, Bangalore
description: "In this talk, we explore the methods and primitives of concurrency across three FP languages: Haskell, Erlang, and Clojure"
tags_str: clojure, haskell, erlang, concurrency, functional-conf, talk
speakerdeck_id: 526c233305b94e9ea8485e562a7ccfa7
speakerdeck_ratio: 1.33333333333333
---
Easy concurrency is one of the main prophesied benefits of the modern functional programming (FP) languages. But the implementation of concurrency differs widely between different FP languages. In this talk, we shall explore the methods and primitives of concurrency across three FP languages: Haskell, Erlang, and Clojure (with core.async).

We shall learn about and compare the trade-offs between

- the green threads and STM channels oriented concurrency of Haskell
- everything-is-a-process and message-passing actor pattern of Erlang
- macro-based CSP of Clojure/core.async
