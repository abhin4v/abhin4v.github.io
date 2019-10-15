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
background-image: url(/slides/fp-patterns/rube-goldberg-machine-cartoon-598.jpg)

???

- rude goldberd machine
  - fragile and opaque
  - things break easily and mysteriously when trying to make changes
- good programmers seek to reduce accidental complexity as much as possible

---
class: title

# Functional Programming

???

- define FP
- outline important tenets of FP
- give a simple and familiar example
- introduce Clojure
- give a simple clojure example

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
abhinavsarkar.net/s/fpp