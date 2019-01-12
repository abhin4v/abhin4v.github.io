---
title: Clojure Concurrency and You
aspect_ratio: '4:3'
---

class: roomy big-h1 no-footer

# Clojure,
# Concurrency,
# And You
.right[## Abhinav Sarkar
```clojure
(((((IN/Clojure Bengaluru 2019)))))
```]

---
class: title fogscreen fullbleed fullcover no-footer big-h1
background-image: url(/slides/clojure-concurrency-you/flipkart_logo.jpg)

# Flipkart

---
class: title fogscreen fullbleed fullcover no-footer big-h1
background-image: url(/slides/clojure-concurrency-you/nilenso.png)

# Nilenso

???

- ask about concurrency experience

---
class: title

# Time

???
So why are we here? We are here because of time.

Without time, programming would be so much simpler.

Here is a sample program without time:

---
class: no-heading

```clojure
(defn pass-time [person]
   (-> person
      (assoc :hair-color :gray)
      (update :age inc)))
```

???
It doesn't matter when you run this code, it'll always return the same answer given a fixed input.

In other words, it's deterministic.

But the real world is not that simple.

In real world, we have values that change over time.

---

# Values and Identities

```clojure
user=> (def abhinav {:hair-color :black :age 33})
#'user/abhinav
```

???
Here `abhinav` is the me inside the program. Its my identity.
Its value is my current hair color and age.

The way it is written now, it is not possible to change the value, because values are immutable in clojure.

--

```clojure
user=> (def abhinav
  #_=>   (atom {:hair-color :black :age 33}))
#'user/abhinav
user=> (swap! abhinav pass-time)
{:hair-color :gray, :age 34}
```

???
`abhinav` is still the identity but its value is set to an atom which is a reference type in clojure which allows us to change the value over time.

In imperative language, this distinction of identity and value is forgotten and the concepts are conflated together as a mutable value.

Mutable values are fine too but soon we have to deal with:

---
class: title

# Concurrency

---

class: quote big

> Concurrency is a program-structuring technique in which there are multiple threads of control which execute "at the same time".

— Simon Marlow, Parallel and Concurrent Programming in Haskell

???

- the effects of the execution of these multiple operations are interleaved
- concurrency is non-deterministic
- concurrency lets us utilize the processing power more effectively
- concurrency allows programs to be modular
- concurrency is not same as parallelism
---

# Threads

- Thread is a sequence of instructions along with a context.
- In case of Clojure on JVM, the threading model is provided by the JVM which only supports OS threads.

???

- Run by processors, Managed by schedulers
- There are different kinds of threads depending on what is scheduling them
- OS threads are scheduled by the OS kernel.
- Without threads, we need to use a event loop mechanism with events and callback
- This makes code very difficult to work with AKA callback hell

---
class: compact fs-75pct

```javascript
fs.readdir(source, function (err, files) {
  if (err) {
    console.log('Error finding files: ' + err);
  } else {
    files.forEach(function (filename, fileIndex) {
      console.log(filename);
      gm(source + filename).size(function (err, values) {
        if (err) {
          console.log('Error identifying file size: ' + err);
        } else {
          console.log(filename + ' : ' + values);
          aspect = (values.width / values.height);
          widths.forEach(function (width, widthIndex) {
            height = Math.round(width / aspect);
            console.log('resizing ' + filename + 'to ' + height + 'x' + height);
            this.resize(width, height).write(dest + 'w' + width + '_' + filename,
              function(err) {
                if (err)
                  console.log('Error writing file: ' + err);
              });
          }.bind(this));
        }
      });
    });
  }
});
```

---
class: no-heading

```clojure
user=> (def t (Thread. #(println "hello")))
#'user/t
user=> (.start t)
hello
nil
```

---
class: no-heading

```clojure
user=> (import java.util.concurrent.Executors)
java.util.concurrent.Executors
user=> (def tp (Executors/newSingleThreadExecutor))
#'user/tp
user=> (.submit tp #(println "hello"))
hello
#object[java.util.concurrent.FutureTask ...]
```

---
class: no-heading

```clojure
user=> (future (println "hello"))
hello
#object[clojure.core$future_call$reify__6962 ...]
```

--

```clojure
user=> (def f (future (do
  #_=>                   (println "hello")
  #_=>                   12345)))
hello
#'user/f
user=> (deref f)
12345
```

???

- both the threadpool version and macro version return us a "future" value
- till the thread is not done, we can wait on the future using `deref`
- when the thread is done, the future resolves to the return value of the thread.
- future is set only once and the value is cached forever
- in all of these examples, printing was done from a thread different than the REPL main thread
- with great concurrency comes great
- **MARKER**: 5 min

---
class: title

# Synchronization

---

# Synchronization

- The process by which multiple threads agree on *some things* at *some time*.
- For example:
  - timing: forking and joining threads
  - value of a variable
  - a sequence of steps to execute
  - access to a shared resource

---
class: pv-7

| ⌄ Time / Things >| One value  | Multiple values     |
|------------------|------------|---------------------|
| Synchronous      | Lock, Atom | Multiple locks, Ref |
| Asynchronous     | Agent      | CRDTs, Raft/Paxos   |

---
class: title

# Locks

---

# Locks

- An easy way of synchronization.
- Prevent concurrent access to critical sections/memory.
- Do not compose.

???

- Threads share memory addresses.
- Locks work like locks on room door. You get inside and lock the door. No one else can get into the room till you unlock it.

---
class: no-heading

```clojure
user=> (def lock (Object.))
#'user/lock
user=> (locking lock
  #_=>   (println "locked hello"))
locked hello
nil
```

---
class: compact no-heading

```clojure
user=> (import java.util.concurrent.locks.ReentrantLock)
java.util.concurrent.locks.ReentrantLock
user=> (def lock (ReentrantLock.))
#'user/lock
user=> (try
  #_=>   (.lock lock)
  #_=>   (println "locked hello")
  #_=>   (finally
  #_=>     (.unlock lock)))
locked hello
nil
```

???

- With locking, we need to be careful about when and in what order to take lock and to release them
- Wrong locking can lead to deadlock, starvation, etc.
- Fortunately, Clojure has many features for supporing concurrency without locks
- **MARKER**: 10 min

---
class: title

# Atoms

---

# Atoms

- Atoms are references which change atomically and immediately.
- Simplest of all reference types.
- Do not compose.

???

- Changes are visible to all threads at the same time.

---
class: no-heading

```clojure
user=> (def abhinav
  #_=>   (atom {:hair-color :black :age 33}))
#'user/abhinav
user=> (swap! abhinav pass-time)
{:hair-color :gray, :age 34}
user=> (reset! abhinav {:hair-color :none :age 33})
{:hair-color :none, :age 33}
user=> @abhinav
{:hair-color :none, :age 33}
```

---
class: compact

```java
package clojure.lang;
import java.util.concurrent.atomic.AtomicReference;

final public class Atom {
  private final AtomicReference state;

  public Atom(Object o) { state = new AtomicReference(o); }

  public Object deref() { return state.get(); }

  public Object swap(IFn f) {
    for (;;) {
      Object v = deref();
      Object newv = f.invoke(v);
      if (state.compareAndSet(v, newv)) {
        notifyWatches(v, newv);
        return newv;
      }
    }
  }
}
```

???

- a wrapper on AtomicReference
- compareAndSet invokes an atomic hardware instruction to set the new value after comparing the current value with passed old value
- if the old value matches, the new value is set and CAS returns true.
- else CAS returns false and for loop try again, infinitely
- CAS is the basic of non-blocking concurrency algorithm and data structures
- something called watches here?

---

# Atom

- Most ubiquitous concurrency feature used in Clojure.
- Use cases: dynamic configs, database connections, simple caches.
- Do not call swap with long running or non-idempotent functions.

???

- need concurrent access? start with atom
- long running functions will get run again and again because short running functions may change the atom value in between
- functions with side-effects may cause the side-effects to execute multiple times
- **MARKER**: 15 min

---
class: title

# Agents

---

# Agents

- Agents, like Atoms, are references which support atomic changes.
- But the changes are made in an asynchronous fashion.
- Do not compose.

---

class: no-heading

```clojure
user=> (def counter (agent 0))
#'user/counter
user=> (dotimes [i 10]
  #_=>   (send counter inc))
nil
user=> (await counter)
nil
user=> (println @counter)
10
nil
```

???
- create an agent initialized to 0
- add x to the current value
- wait until all sent actions are done
- should have the answer
- an agent hold an internal state like atom
- like swap in atoms, you can send a function to an agent
- but the functions send to agents are not executed immediately
- instead they are put in a queue in the order they are sent to the agent
- and executed one by one, never concurrently
- this makes sure that functions are never executed multiple times like atom

---

# Agents are not Actors

```erlang
-module(counter).
-export([loop/1]).

loop(N) -> receive
  {inc} -> loop(N+1);
  {get, Sender} -> Sender ! N, loop(N)
end.

> Pid = spawn(counter, loop, [0]).
> Pid ! {inc}.
> Pid ! {get, self()}.
> receive Value -> io:fwrite("~p~n", [Value]) end.
1
```

???

- Actor only has certain behaviors which are specific by the actor itself
- Agent can behave in any way depending on the function sent to it
- Only way to get the value inside an actor is to send it a message with another actor's id to which it can send its value to.
- Agent state is directly accessible at all times
- Agents are reactive, not autonomous - there is no message loop and no blocking receive.
- Actor and agent both have an internal queue for the functions and messages sent to them respectively
- pulsar is a library to use erlang like actors in clojure

---

# Agents

- Can be used for any state that does not require strict consistency for reads:
  - Counters (e.g. message rates in event processing)
  - Collections (e.g. recently processed events)
- Can be used for offloading arbitrary computations to a thread pool using `send-via`.
- Uses an unbounded queue, so too many functions enqueued in it may cause OOM.

???

- send is backed by a fixed thread pool of size 2 * number of processors
- so use send for only short running functions
- if the function is expected to run for long time, use send-off instead which runs it on a cached thread pool
- So we have seen two ways of handling individual references. What if we want to change multiple of them at once? Are we left to the mercy of multiple locks and issues that come with them? No! In clojure, we have:
- **MARKER**: 20 min

---
class: title

# Refs

---
class: compact

# Refs and Software Transactional Memory

- Refs allows changing multiple references together in a single atomic operation.
- **Atomicity**: All the state changes become visible to all the threads at once.
- **Consistency**: All the state changes can be validated before allowing the transaction to commit.
- **Isolation**: The atomic operation is completely unaffected by whatever other threads are doing.

???
 - No threads ever see inconsistent half-done states.
 - It is as if STM operation takes a snapshot of the state of the world when it begins running, and then executes against that snapshot.
 - Let's see a comparison with multiple locks.

---

![Bank Transfer](/slides/clojure-concurrency-you/bank_transfer.png# db pt-6)

???

- Let's take this example of transferring money between two accounts.
- There can be a concurrent transfer in reverse direction too.

---
class: compact fs-85pct

```java
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Account {
    private int id, amount;
    private Lock lock = new ReentrantLock();

    Account(int id, int initialAmount) {
        this.id = id;
        this.amount = initialAmount;
    }

    public void withdraw(int n) {
        this.lock.lock();
        try { this.amount -= n; }
        finally { this.lock.unlock(); }
    }

    public void deposit(int n) { this.withdraw(-n); }

    public void transfer(Account other, int n) {
        this.withdraw(n);
        other.deposit(n);
    }
}
```

???

- You may think that adding a lock like this will ensure safety but this transfer function is wrong.
- in case of a concurrent reverse transfer, you'll end up with a deadlock.

---
class: compact fs-85pct

```java
public void transfer(Account other, int n) {
    if (this.id < other.id) {
        this.lock.lock();
        other.lock.lock();
    } else {
        other.lock.lock();
        this.lock.lock();
    }
    try {
        this.amount -= n;
        other.amount += n;
    } finally {
        if (this.id < other.id) {
            this.lock.unlock();
            other.lock.unlock();
        } else {
            other.lock.unlock();
            this.lock.unlock();
        }
    }
}
```

???

- the right way to do this is to acquire the locks in a deterministic order
- in this case, the account id is used to make it sure
- also note that you cannot reuse the existing deposit and withdraw functions because they release the lock.
- this is too complicated and error prone and non-extensible

---
class: no-heading

```clojure
(def account1 (ref 100))
(def account2 (ref 100))

(defn withdraw [account amount]
  (alter account - amount))

(defn deposit [account amount]
  (withdraw account (- amount)))

(defn transfer [from to amount]
  (dosync
    (withdraw from amount)
    (deposit to amount)))
```

---

![](/slides/clojure-concurrency-you/clojure-stml.png# db w-100pct ph-4)

.footer[
- Source: https://www.sw1nn.com/blog/2012/04/11/clojure-stm-what-why-how/
]

???

- So how does STM work?
- there are many ways to do STMs
  - locking/pessimistic
  - lock-free/optimistic
  - MVCC
- Clojure uses MVCC
- When a transaction is started via `dosync` the current values of the refs are captured in the transaction snapshot.
- These captured values are the value that (deref) will return for the duration of the transaction.
- `alter` changes the captured values in the snapshot only
- when the transaction commits, the values of refs changed with the transaction are compared to the actual values of the refs
- if all the values are same, the transaction commits and the values in transaction log are written to the refs atomically.
- else the transaction aborts and is retried automatically
- there is no check done of read-only refs unless explicitly asked for by calling `ensure`
- the transaction may be retried any number of times so do not do side-effecting things in them

---

![](/slides/clojure-concurrency-you/ref_history.png# db w-100pct ph-6 pv-6)

.footer[
- Source: https://mattias.niklewski.com/2014/04/stm.htm
]

???

- A ref is a wrapper around a history of values, implemented as a doubly linked list. Each node contains a value and a write point, which is a unique timestamp.
- Reading from a ref involves walking down its history list until a value is found that predates the transaction's starting point.
- But the history lists are not of infinite length. By default they grow dynamically up to length 10. If a transaction cannot find a value with an early enough write point, it retries with an updated start point.
- `commute` used like alter but does not cause write collisions. The idea is that if two transactions both alter a ref using a commutative function, the result is the same regardless of who commits first.

---
class: compact

# Clojure STM vs. Haskell STM

```haskell
import System.IO
import Control.Concurrent.STM

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
  bal <- readTVar acc
  writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (- amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = atomically $ do
  deposit to amount
  withdraw from amount
```

???

- Haskell STM is optimistic instead of MVCC
- Haskell matches values read in a transaction too
- Haskell STM gives capability to programmer to conditionally retry a transaction
- Haskell type system ensures that you can't do IO in STM
- Haskell STM gives an `orElse` function to run a different STM action first one fails
- Haskell's algorithm guarantees true serializable isolation which is stronger than snapshot isolation which Clojure guarantees

---
class: compact

# Refs

- Best option for implementing in-memory transaction data stores
  - Chat servers
  - Multiplayer games
- In-memory stream computation solutions.
- A long-running transaction may re-execute many times because it may be repeatedly aborted by shorter transactions.
- Keeping history of values is expensive. Even more so when the values are not persistent collections.

???

- **MARKER**: 30 min

---
class: title

# core.async

---

# core.async and Communicating Sequential Processes

- Independent threads of activity.
- Synchronous communication through channels.
- Multiplexing of channels with alternation.

---

![CSP](/slides/clojure-concurrency-you/csp.png# db w-100pct pv-5)

.footer[
  - Source: arild.github.io/csp-presentation]

???
- this image shows three processes running concurrently
- process 1 and 2 are using a channel to communicate
- process 1 reads from the empty channel and it blocks
- later, process 2 writes to the channel and p1 resumes with the value that p2 wrote
- similarly, p2 and p3 are using a common channel to communicate.
- p2 writes to the channel and blocks till p3 reads from it later.
- read and writes to channels server as synchronization points.

---
class: no-heading

```clojure
(go (println "hi"))

(def echo-chan (chan))
(go (println (&lt;! echo-chan)))
(go (>! echo-chan "hello"))
; => hello

(def echo-chan (chan 10))
```

---
class: no-heading

```clojure
(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (dotimes [n 3]
    (go
      (let [[v ch] (alts! [c1 c2 c3])]
        (println "Read" v "from" ch))))
  (go (>! c1 "hello"))
  (go (>! c2 "allo")
  (go (>! c2 "hola")))
; => Read allo from #<ManyToManyChannel ...>
; => Read hola from #<ManyToManyChannel ...>
; => Read hello from #<ManyToManyChannel ...>
```

???

- core.async is based on deep-walking macros.
- go is a deep-walking macro that walks recursively through the entire code inside go blocks and transforms it into a state-machine
- essentially, it compiles clojure code into a state machine form.
- go threads are actually run on kernel threads.
- but when they encounter a parking operation like putting into or getting from a channel, they are "parked".
- at this point, the entire state of the stack including the local variables, the function reference, the current state and the state to resume at, everything is put into an array, wrapped into a callback function and put into the channel's internal queue.
- when the corresponding resume event happens, put for get and get for put, the channel calls the callback, resuming the go-thread on some kernel thread.
- this gives an illusion of green threads.
- alts! is a little complicated with channel locking the callbacks using a shared lock.
- I don't want to get into the internals of core.async anymore than this.
- But I do want to show-off some great combinators which come with it.

---
class: fs-85pct

# Multiplexing

```clojure
user=> (def input (chan 1))
#'user/input
user=> (def broadcast (mult input))
#'user/broadcast
user=> (dotimes [i 3]
  #_=>   (let [output (chan 1)]
  #_=>     (tap broadcast output)
  #_=>     (go-loop []
  #_=>       (if-let [v (&lt;! output)]
  #_=>         (do (println i "Got!" v)
  #_=>             (recur))
  #_=>         (println "Exiting")))))
nil
```

---
class: fs-85pct

# Multiplexing

```clojure
user=> (>!! input 42)
true0 Got! 421

2 Got! Got!42
42
user=> (>!! input 43)
true
01  Got!Got!  4343
2
Got! 43
user=> (close! input)
Exitingnil

Exiting
Exiting
```

???

---
class: fs-75pct

# Publish-Subscribe

```clojure
user=> (def input (chan 1))
#'user/input
user=> (def p (pub input :tag))
#'user/p
user=> (let [c (chan 1)]
  #_=>   (sub p :cats c)
  #_=>   (go-loop []
  #_=>     (if-let [v (&lt;! c)]
  #_=>       (println "Cat guy got:" v)
  #_=>       (println "Cat guy exiting"))))
#object[clojure.core.async.impl.channels.ManyToManyChannel ...]
user=> (let [c (chan 1)]
  #_=>   (sub p :dogs c)
  #_=>   (go-loop []
  #_=>     (if-let [v (&lt;! c)]
  #_=>       (println "Dog guy got:" v)
  #_=>       (println "Dog guy exiting"))))
#object[clojure.core.async.impl.channels.ManyToManyChannel ...]
```

---
class: fs-85pct

# Publish-Subscribe

```clojure
user=> (defn send-with-tags [msg]
  #_=>   (doseq [tag (:tags msg)]
  #_=>     (println "sending... " tag)
  #_=>     (>!! input {:tag tag :msg (:msg msg)})))
#'user/send-with-tags
user=> (send-with-tags {:msg "New Cat Story" :tags [:cats]})
sending...  :cats
nil
Cat guy got: {:tag :cats, :msg New Cat Story}
user=> (send-with-tags {:msg "New Dog Story" :tags [:dogs]})
sending...  :dogs
nil
Dog guy got: {:tag :dogs, :msg New Dog Story}
```

---
class: fs-85pct

# Publish-Subscribe

```clojure
user=> (let [c (chan 1)]
  #_=>   (sub p :dogs c)
  #_=>   (sub p :cats c)
  #_=>   (go-loop []
  #_=>     (if-let [v (&lt;! c)]
  #_=>       (println "Cat/Dog guy got:" v)
  #_=>       (println "Cat/Dog guy exiting"))))
#object[clojure.core.async.impl.channels.ManyToManyChannel]
```

???

- there are also merge and mix combinator to go reserve of mutliplexing and pub-sub, they allow us to join muliple channels into a single channel
- merge is static after creation whereas mix allows more dynamic operations

---

# Pipelines

```clojure
(pipeline n to xf from close? ex-handler)
(pipeline-async n to af from)
(pipeline-blocking n to xf from close? ex-handler)
```

???

- Takes elements from the from channel and supplies them to the to channel, subject to the transducer xf, with parallelism n.
- other than these, channels also support the usual map and filter functions
- We can create complete data transformation pipelines using these combinators

---
class: compact col-2
# core.async vs Goroutines

```go
func fibonacci(c, q chan int) {
  x, y := 0, 1
  for {
    select {
    case c <- x:
      x, y = y, x+y
    case <-q:
      fmt.Println("quit")
      return
    }
  }
}
```

```go
func main() {
  c := make(chan int)
  quit := make(chan int)
  go func() {
    for i := 0; i < 10; i++ {
      fmt.Println(<-c)
    }
    quit <- 0
  }()
  fibonacci(c, quit)
}
```

???

- go concurrency is built into the runtime where core.async is just a library based on macros
- go uses actual green threads while c.a simulated green threads over OS threads
- go has async IO built into the runtime while with clojure you need to use special libraries for it. c.a go thread can block if you do blocking IO in them
- go has better scheduling algorithms for goroutines whereas c.a does no intelligent scheduling
- c.a comes with combinators for ease of programming

---

# core.async

- Data transformation pipelines like ETL
- Multi-user chat servers, game servers
- More broadly, Staged Event Driven Architecture programs
- With Clojurescript, it is a great replacement for callback for UI interactions

--
- Doing blocking IO in go-threads blocks them.
- Error handling is complicated.
- Too many pending puts or take may throw errors.

???
- **MARKER**: 40 min

---
class: no-footer center

![clojure](/slides/clojure-concurrency-you/Clojure.png# w-75pct)

???

- clojure provides great features to simplify concurrent programming
- Use them to structure your programs at a higher level than locks
- Different techniques are suitable for different situations.

---

# References

- [Clojure reference documentation](https://clojure.org/reference/documentation)
- [Clojure STM: What, why, how?](https://www.sw1nn.com/blog/2012/04/11/clojure-stm-what-why-how/)
- [Thoughts on STM](https://mattias.niklewski.com/2014/04/stm.html)
- [Implementation details of core.async Channels](https://vimeo.com/100518968)
- [Core Async Go Macro Internals](https://www.youtube.com/watch?v=R3PZMIwXN_g)
- [Publish and Subscribe with core.async](https://yobriefca.se/blog/2014/06/04/publish-and-subscribe-with-core-dot-asyncs-pub-and-sub/)

---

class: big-h1 no-footer center mr-5 thank-you ph-1

# Thank You
## @abhin4v
abhinavsarkar.net/talks/clojure-concurrency-you