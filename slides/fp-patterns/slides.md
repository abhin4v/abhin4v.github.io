---
title: "Functional Programming Patterns"
aspect_ratio: '4:3'
---

class: roomy big-h1 no-footer bgs-contain
background-image: url(/slides/fp-patterns/lambda.svg)

.ff-fancy[
# Functional Programming Patterns]
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
class: title fogscreen
background-image: url(/slides/fp-patterns/complex.jpg)

# Complexity

.footer[
  - Credit: [flic.kr/p/d2xjKN](https://flic.kr/p/d2xjKN)
]

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
class: title fogscreen
background-image: url(/slides/fp-patterns/circuit.jpg)

# Functional Programming

.footer[
  - Credit: [flic.kr/p/5V7C8S](https://flic.kr/p/5V7C8S)
]

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
class: title fogscreen
background-image: url(/slides/fp-patterns/pattern.jpg)

# Functional Programming Patterns

.footer[
  - Credit: [flic.kr/p/3erjC9](https://flic.kr/p/3erjC9)
]

---

# Functional Programming Patterns

1. Immutable Data; Explicit State
1. Dataflow Programming
1. Explicit over Implicit
1. Data-oriented Programming
1. Functional Core; Imperative Shell

---
class: title
# Immutable Data; <br> Explicit State

---

# Immutable Data

.bg-caption[What?]
- Objects which once created cannot be changed.
- All domain objects should be immutable:
  - User, Account, Transaction, Event etc.

---

# Immutable Data

.bg-caption[Why?]
- No sneaky action-at-distance.
- Thread safety.
- Easier to reason about.
- GC is quite fast now. Creating objects is cheap.

---

# Immutable Data

.bg-caption[How?]
- In Clojure, just use built-in data structures like vector and map.
- In Java, use Google `@AutoValue` or Lombok `@Value` annotations.
- Clojure data structures share memory so they are more memory efficient.

---
class: compact

# Immutable Data: Java

```java
import lombok.Value;
import lombok.With;

@Value
public class Person {
  private String name;
  @With private int age;
  private double score;
}

Person abhinav = new Person("abhinav", 30, 100.0);
String name = abhinav.getName();
Person olderAbhinav = abhinav.withAge(31);
```

---
class: compact

# Immutable Data: Clojure

```clojure
=> (def abhinav {:name "Abhinav" :age 30 :score 100.0})
{:name "Abhinav", :age 30, :score 100.0}

=> (def name (:name abhinav))
"Abhinav"

=> (def older-abhinav (assoc abhinav :age 31))
{:name "Abhinav", :age 31, :score 100.0}
```

---

# Explicit State

.bg-caption[What?]
- Everything is immutable by default.
- Mutable state is marked explicitly so.
- Examples are thread pools, connection pools, dynamic configs, caches etc.

---

# Explicit State

.bg-caption[Why?]
- Mutability is constrained. You know the possible sources of sneaky action-at-distance.
- Proper thread safety implemented for these specific mutability constructs.
- Easier to reason about.

---

# Explicit State

.bg-caption[How?]

  - In Clojure, use [`atom`](https://clojure.org/reference/atoms), [`agent`](https://clojure.org/reference/agents) or [`ref`](https://clojure.org/reference/refs).
  - In Java, use [`AtomicReference`](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/atomic/AtomicReference.html) with immutable value, [`Quasar`](http://www.paralleluniverse.co/quasar/) or [`Akka`](https://doc.akka.io/docs/akka/current/index-actors.html) actors with inaccessible mutable state.
  - [VAVR library](https://www.vavr.io/) provides persistent DS like Clojure for Java.

---
class: compact

# Explicit State: Clojure

```clojure
=> (def abhinav (atom {:hair-color :black :age 33}))
#'user/abhinav
=> (swap! abhinav pass-time)
{:hair-color :gray, :age 34}
=> (reset! abhinav {:hair-color :none :age 33})
{:hair-color :none, :age 33}
=> @abhinav
{:hair-color :none, :age 33}
```

--

```clojure
(defn pass-time [person]
   (-> person
      (assoc :hair-color :gray)
      (update :age inc)))
```

---
class: compact

# Explicit State: Java

```java
class Person {
  private String hairColor;
  private int age;
  public Person(String hairColor, int age) {
    this.hairColor = hairColor;
    this.age = age;
  }
  public Person withAge(int age) {
    return new Person(this.hairColor, age);
  }
  public Person withHairColor(String hairColor) {
    return new Person(hairColor, this.age);
  }
  public Person passTime() {
    return this.withHairColor("Gray").withAge(this.age + 1);
  }
}
```

---
class: compact

# Explicit State: Java

```java
AtomicReference<Person> abhinav =
      new AtomicReference(new Person("Black", 33));
abhinav.updateAndGet(Person::passTime);
abhinav.set(new Person(null, 33));
abhinav.get();
```

---
class: title
# Dataflow Programming

---
class: title
# Explicit over Implicit

---
class: title
# Data-oriented <br> Programming

---
class: title
# Functional Core; <br> Imperative Shell

---

class: title fogscreen
background-image: url(/slides/fp-patterns/simple.jpg)

![](/slides/fp-patterns/rich.jpeg# absolute r-2 t-30pct w-30pct)

---

# References

- [No Silver Bullet —Essence and Accident in Software Engineering](http://worrydream.com/refs/Brooks-NoSilverBullet.pdf)
- [Why Functional Programming Matters](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)
- [Translating an Enterprise Spring Webapp to Clojure](https://blog.jakubholy.net/translating-enterprise-spring-app-to-clojure/)
- [Functional Core; Imperative Shell](https://gist.github.com/kbilsted/abdc017858cad68c3e7926b03646554e)
- [Simple Made Easy](https://www.infoq.com/presentations/Simple-Made-Easy/)

---
class: big-h1 no-footer center mr-5 thank-you ph-1

.ff-fancy[
# Thanks]
## [@abhin4v](https://twitter.com/abhin4v)
abnv.me/fpp
![QR code](/slides/fp-patterns/qr.png# center)