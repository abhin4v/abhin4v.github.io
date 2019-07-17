---
title: "Continuation Defunctionalization: Deriving Binary Tree Iterators Mechanically"
date: 2019-08-17
description: Deriving tree iterators from tree traversals using Continuation Defunctionalization.
tags: java, programming, algorithm
author: Abhinav Sarkar
toc: right
---

Binary tree is the simplest of tree data structures. It is a tree in which each node has at most two children. A tree traversal is a process of visiting each node in the tree, exactly once. There are [multiple ways of traversing][1] a [binary tree]{.w} with each traversal resulting in a different enumeration of the tree elements. These tree traversals can be defined as simple recursive functions. But what if we want to write [Java-style iterators] for them? And is there a way to derive these iterators mechanically from the simple traversal functions? Let's find out.

<!-- more -->

* toc

## Traversals and Iterations

This is a sample binary tree:

```plain
          D
        /   \
       C      F
      / \    / \
     A   B  E   G
```

Different traversals will yield different sequences of this tree:

Traversal   Output
----------  -------
in-order    ACBDEFG
pre-order   DCABFEG
post-order  ABCEGFD

The code for the recursive in-order traversal (traverse left child, then self, then right child) is very simple. Assuming a binary tree is represented with a `Tree` class like:

```java
class Tree<T> {
  Tree<T> left;
  T content;
  Tree<T> right;
}
```

this code prints all elements of a tree in in-order:

```java
static <T> void printRecursive(Tree<T> tree) {
  if (tree != null) {
    printRecursive(tree.left);
    System.out.print(tree.content);
    printRecursive(tree.right);
  }
}
```

But the code for an iterator which iterates a tree in in-order is much more complicated:

```java
class InOrderIterator<T> implements Iterator<T> {
  private Tree<T> tree;
  private Stack<Tree<T>> stack = new Stack<>();

  InOrderIterator(Tree<T> tree) { this.tree = tree; }

  @Override
  public boolean hasNext() {
      return tree != null || !stack.isEmpty();
  }

  @Override
  public T next() {
    while (hasNext()) {
      if (tree != null) {
        stack.push(tree);
        tree = tree.left;
      } else {
        if (!stack.isEmpty()) {
          Tree<T> t = stack.pop();
          T content = t.content;
          tree = t.right;
          return content;
        }
      }
    }
    throw new NoSuchElementException();
  }
}

class Main {
  public static void main(String[] args) {
    Tree<String> tree = new Tree<>(...);
    InOrderIterator<String> inOrderIterator = new InOrderIterator<>(tree);
    while (inOrderIterator.hasNext()) {
      System.out.print(inOrderIterator.next());
    }
  }
}
```

The iterator code uses a `Stack` to simulate the program stack of the recursive traversal. It takes some thinking about the tree structure and the program flow to write this code and it is easy to get it wrong[^int-ext-iterators]. [Pre-order] and [Post-order] iterators are even more complicated. Is there a way to write the iterators starting from the recursive traversals mechanically by following some rules? There is indeed! Keep reading for the details.

## Binary Tree

Let's start with some setup code:

```java
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Consumer;
import java.util.function.Supplier;

class Utils {
  static <T> void printContent(T t) {
    System.out.printf(t + " ");
  }

  static String generateRandomAlphaString(int maxLength) {
    ThreadLocalRandom random = ThreadLocalRandom.current();
    int targetLength = random.nextInt(maxLength) + 1;
    StringBuilder sb = new StringBuilder(targetLength);
    for (int i = 0; i < targetLength; i++) {
      sb.append((char) random.nextInt('a', 'z' + 1));
    }
    return sb.toString();
  }

  static StringBuilder makeGuidelines(int times) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < times; i++) {
      sb.append('│');
    }
    return sb.append("├ ");
  }
}
```

With these imports and utility functions out of our way, let's first define a Binary Tree:

```java
class Tree<T> {
  Tree<T> left;
  T content;
  Tree<T> right;

  Tree(Tree<T> left, T content, Tree<T> right) {
    this.left = left;
    this.content = content;
    this.right = right;
  }

  public static <T> Tree<T> generate(
      int maxDepth, Supplier<T> gen, double nullProbability) {
    if (nullProbability < 0 || nullProbability > 1) {
      throw new IllegalArgumentException("nullProbability must be between 0 and 1");
    }

    if (maxDepth == 0) {
      return null;
    }

    double rand = ThreadLocalRandom.current().nextDouble();
    if (rand < nullProbability) {
      return null;
    }

    Tree<T> left = generate(maxDepth - 1, gen, nullProbability);
    Tree<T> right = generate(maxDepth - 1, gen, nullProbability);
    return new Tree<>(left, gen.get(), right);
  }

  @Override
  public String toString() {
    return toStringLayout(0).toString();
  }

  private StringBuilder toStringLayout(int level) {
    StringBuilder guidelines = Utils.makeGuidelines(level);
    StringBuilder sb = 
      new StringBuilder().append(guidelines).append(content).append("\n");
    if (this.left == null && this.right == null) {
      return sb;
    }

    if (this.left != null) {
      sb.append(this.left.toStringLayout(level + 1));
    } else {
      sb.append(guidelines).append("<NULL>\n");
    }

    if (this.right != null) {
      sb.append(this.right.toStringLayout(level + 1));
    } else {
      sb.append(guidelines).append("<NULL>\n");
    }

    return sb;
  }
}
```

Each node in a binary tree has some content and two nullable child nodes. We have a constructor to create the tree. `generate` function generates an arbitrary binary tree of a given maximum depth by using a random content generator. We will used this function to generate trees to test our traversals and iterators.

We also implement the `toString` method for the tree to create a string representation of the tree to help us in debugging. A sample run:

```java
Tree<String> tree = Tree.generate(4, () -> Utils.generateRandomAlphaString(2), 0.1);
System.out.println(tree);
```

Output:

```plain
├ ji
│├ r
││├ r
│││├ c
│││├ ki
││├ vf
│││├ ti
││├ <NULL>
│├ d
││├ dg
│││├ eo
│││├ x
││├ si
│││├ bt
│││├ ee
```

## Recursive Traversal

Credits first: this blog post and the code in it is inspired by [The Best Refactoring You've Never Heard Of][2] article (and talk) by James Koppel. In the talk, James shows how to transform a recursive in-order traversal into an iterative one. For this post, I've chosen to implement pre-order and post-order iterators.

Let's start with the recursive pre-order traversal which prints the content of every node in the tree in pre-order — each node's content is printed before its children's.

```java
static <T> void printRecursive(Tree<T> tree) {
  if (tree != null) {
    Utils.printContent(tree.content);
    printRecursive(tree.left);
    printRecursive(tree.right);
  }
}
```

Short and sweet, simple and easy to understand. If the tree is not null, we print its content, then we recursively print the left and right child trees.

First thing to do is to extract out the print action into a function argument so that we can do different kinds of actions on the nodes instead of just printing them:

```java
static <T> void iterateRecursive(Tree<T> tree, Consumer<T> action) {
  if (tree != null) {
    action.accept(tree.content);
    iterateRecursive(tree.left, action);
    iterateRecursive(tree.right, action);
  }
}
```

We use a [`Consumer`] for the type of the action. Since `Consumer` is a functional interface, we can pass lambdas in its place. We can call it like this:

```java
iterateRecursive(tree, Utils::printContent);
```

This transformation was easy to grok. The next one requires a little head-tilting. We convert the simple recursion into a _Continuation-passing style_ (CPS) recursion:

```java
static <T> void iterateCPS(Tree<T> tree, Consumer<T> action, Runnable cont) {
  if (tree != null) {
    action.accept(tree.content);
    iterateCPS(tree.left, action, () -> iterateCPS(tree.right, action, cont));
  } else {
    cont.run();
  }
}
```

So what is _Continuation-passing style_?

## Continuations

In the usual direct [imperative programming]{.w} style, we write one statement after another, as a sequence of steps to execute. There is another way of thinking about it: after returning from executing one statement, the rest of the program — which can be thought of as a big statement itself — is run. In _Continuation-passing style_, this way is made explicit: each statement takes the rest of the program which comes after it as an argument, which it invokes explicitly. For example, if you have a program written in direct style like this:

```java
static int start(String s) {
  int a = getSomething(s);
  doAnotherThing(a);
  return a;
}
```

It can be converted into an equivalent CPS style program like this:

```java
static void startCPS(String s, Callable<Integer> cont) {
  getSomethingCPS(s, (a) -> {
    doAnotherThingCPS(a, () -> {
      cont.call(a);
    });
  });
}

static void getSomethingCPS(String s, Callable<Integer> cont) {
  cont.call(getSomething(s));
}

static void doAnotherThingCPS(int a, Runnable cont) {
  doAnotherThing(a);
  cont.run();
}
```

We see how each function call takes the rest of the program after it as a lambda and calls it explicitly to further the flow of the program. Instead of returning an `int` value, the `startCPS` function now takes a lambda as an additional argument which it calls with the `int` value at the end of all the processing. These lambdas are known as _Continuations_ because they **continue** the flow of the programs, and hence this style of writing programs is called the _[Continuation-passing style]{.w}_.



[^int-ext-iterators]: These traversals can be generalized to take a function which they call with the content of each node. Such traversals are examples of [Internal Iterators]. [Java-style iterators] on the other hand are examples of [External Iterators].

[1]: https://en.wikipedia.org/wiki/Tree_traversal
[2]: http://www.pathsensitive.com/2019/07/the-best-refactoring-youve-never-heard.html
[Java-style iterators]: https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/Iterator.html
[Pre-order]: https://www.geeksforgeeks.org/iterative-preorder-traversal/
[Post-order]: https://www.geeksforgeeks.org/iterative-postorder-traversal-using-stack/
[Internal Iterators]: https://en.wikipedia.org/wiki/Iterator#Internal_Iterators
[External Iterators]: https://en.wikipedia.org/wiki/Iterator#External_iterators_and_the_iterator_pattern
[`Consumer`]: https://docs.oracle.com/en/java/javase/12/docs/api/java.base/java/util/function/Consumer.html