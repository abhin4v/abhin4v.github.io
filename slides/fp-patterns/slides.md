---
title: "Functional Programming Patterns"
aspect_ratio: '4:3'
---

class: roomy big-h1 no-footer

# Functional Programming Patterns
.right[## Abhinav Sarkar
  ## Oct 2019
]

???

- ask about java and functional programming experience

---
class: center pt-6

# Flipkart
# nilenso
# Capillary
# FICO

---
class: title

# Complexity

???

- what is the biggest challenge for programmers these days?
- it's complexity
- thanks for Moore's law and improvements in compilers and runtimes, we don't need to care about nitty-gritties of performance of most of the programs we write anymore
- and thanks to the same, the computers are running the world now.
- which has lead to more and more complex things being implemented in software.

---

# Complexity

- Complexity is the degree of entanglement in software.
- Complex programs are hard to understand and hence hard to change.

???

- I bet you have seen complex codebases
- These are generally codebases which no one wants to touch
- Development of these projects get slower and slower and finally you do a massive rewrite hoping that it'll make working on these projects fast again
- Fred Brooks discussed complexity in great details in his 1986 paper titled "No Silver Bullet – Essence and Accident in Software Engineering".
- In the paper, Fred classifies complexity in two categories: Essential and Accidental.

---

# Essential Complexity

- Required for solving the problem at hand.
  - Like implementing Breadth-first search algorithm for finding the shortest path.
  - It is unavoidable regardless of which tools and frameworks you use.

---

# Accidental Complexity

- Not inherent to the problem; brought while writing the program.
--

  - Choose to use _C_? Now you have to manage memory too.
--

  - Choose to use _Java_? Now you have to model everything as objects.
--

  - Choose to use _node_? Now you have to deal with 3 million dependencies.

---
class: img-caption

![](/slides/fp-patterns/rube-goldberg-machine-cartoon-598.jpg)

Rube Goldberg Machine

???

- rude goldberd machine
  - fragile and opaque
  - things break easily and mysteriously when trying to make changes
- good programmers seek to reduce accidental complexity as much as possible
- i claim that functional programming helps in reducing accidental complexity as compared to OOP

---
class: title

# Functional Programming

---

# Functional Programming

> Functional programming is a programming paradigm that treats computation as the evaluation of mathematical functions and avoids changing-state and mutable data.

---

# FP: Salient Aspects

- pure functions
???
- a function is pure if it's output depends only on it's input and nothing else
- easier to reason about and debug because they don't depend on mutable state
- easier to test because there are no dependencies
- §
--

- first-class and higher-order functions
???
- first-class functions can be passed to and returned from functions
- functions which take or return functions are called higher-order functions
- §
--

- no/constrained mutable state
???
- mutable state leads to coupling between components
- mutable state requires synchronization when working with multiple threads
- FP eschews mutable state
- FP languages are immutable by default and mutability is opt-in by using specific constructs
- §
--

- no/constrained side-effects
???
- side effects are any changes in state that do not depend on the function inputs
- IO is side effect too
- code with side effects is harder to reason about
- FP languages provide specific constructs to constrain side effects
- §
--

- optionally, lazy evaluation and static type system
???
- some FP languages come with lazy evaluation and/or static type systems

---
```javascript

let meetups =
  [{name: 'JS', isActive: true, members: 700},
   {name: 'Clojure', isActive: true, members: 900},
   {name: 'Java', isActive: false, members: 600},
   {name: 'Go', isActive: true, members: 500}];

let totalMembers = meetups
  .filter(m => m.isActive)
  .map(m => m.members)
  .reduce((acc, m) => acc + m, 0);
```

---
class: title fogscreen
![](/slides/clojure-concurrency-you/Clojure.png# absolute t-0 center w-75pct)

# Clojure
---

# Clojure
--

- dynamically typed
--

- functional programming language
--

- from the Lisp family
--

- runs on JVM

---

# Clojure

```clojure
(def meetups
  [{:name "JS" :is-active true :members 700}
   {:name "Clojure" :is-active true :members 900}
   {:name "Java" :is-active false :members 600}
   {:name "Go" :is-active true :members 500}])

(def total-members
  (->> meetups
    (filter :is-active)
    (map :members)
    (reduce (fn [acc m] (+ acc m)) 0)))
```

---

# Functional Programming Patterns

1. Immutable Data; Explicit State
1. Dataflow Programming
1. Explicit over Implicit
1. Common Data Structures over Custom Objects
1. Data-oriented Programming
1. Functional Core; Imperative Shell

---
class: title
# Immutable Data; <br> Explicit State

---
class: title
# Dataflow Programming

---
class: title
# Explicit over Implicit

---
class: title
# Common Data Structures <br> over <br> Custom Objects

---
class: title
# Data-oriented Programming

---
class: title
# Functional Core; <br> Imperative Shell

---

class: big-h1 no-footer center mr-5 thank-you ph-1

# Thanks
## [@abhin4v](https://twitter.com/abhin4v)
abnv.me/fpp
![QR code](https://chart.googleapis.com/chart?chs=200x200&cht=qr&chl=http://abnv.me/fpp&choe=UTF-8# center)