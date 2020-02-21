---
title: Coding for Humans
date: 2020-01-13
info: Coding is hard, it seems. I frequently face the same problems.
draft: false
---

Coding is hard,
well it seems so, because I (and others) have usually a hard time to read code from others.
This is not always the case of course. But it seems that over the time readable code
from different people appears to me readable because of the same aspects.
In my experience readability is not related to specific languages.

<!-- link zu dem typen der gesagt hat das jeder programmieren kann nur programmieren können so programmieren das andere es verstehen -->

So what makes code readable?
It depends on a lot of factors, but in this article we talk about
variable names and context.

## Context

With context I mean amount of information you have to "carry"
to understand the code in front of you.
The simple rule here is the more context needed the harder
the code is to understand.

Clearly we can't reduce serious programs to the bare minimum
of what the programming languages has to offer, we use libraries
and frameworks. Also programs tend to create it's own context
which developers have to understand to make larger amount of changes.

Keeping the context needed to understand your program small is the art form.
There are various methods on how to do that.

* don't write *smart* programs, write simple programs.
* use proper variables names.
* write modular code.
* fail fast
* do not queue functions, fall them.

## Variable names

Proper variable and function names make it easy for people to read your code.
So let's talk about what are not proper names,
on letter variables sort forms or letters left out words are
in general no proper names.

For examples:

`ctx`, `req`, `res`, ... 

might seem to be familiar and kind of a standard, but reading something
like 

`context`, `request`, `result`, ...

does not leave any questions open.


### One letter variable names are a no go

The worst way to name your variables or functions 
is by just using one letter.

```
let w = WriteHandle::new("./some/path","w");
w.write("some string");
```

might seem legit in the first glance, but imagine you don't have the first line as information.

What's wrong with :

```
let writerHandle = WriteHandle::new("./some/path","w");
writeHandle.write("some string");
```

## motto

If you think about if you can shorten the variable or function name, 
you are just wasting time. Just do it always, and you never run in problems.



# Functions Queue and Functions calling functions

## Functions Queue

* functionA calls functionB
* functionB calls functionC 
* functionC calls functionD

is bad, becaus this is basically one function.

## Functions

* functionA calls functionB
* functionA calls functionC
* functionA calls functionD

This is good.

# Use variables to increase readability

Readability is key. One-Liners are not an Art-form, even if you use comments to describe what you are doing.

Example, lambdas in haskell.
