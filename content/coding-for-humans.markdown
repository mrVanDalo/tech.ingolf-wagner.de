---
title: Coding for Humans
date: 2020-01-13
info: Coding is hard, it seems. I frequently face the same problems.
draft: true
---

# Coding is hard

well it seems so, because I (and others) have usually a hard time to read code from others.
This is not always the case of course. But it seems that over the time readable code
from different people apears to me readable because of the same aspects.
In my experiance readability is not related to a Language.
It is more related to things like documentational writing style, patterns and testing.


## Writing tests
Testing your code intensively usually results in a robust and modular,
which is easy to read and to improve.

So use your testing framework and get familiar with concepts like Dependency Injection, mocking and TDD.



# Variable names

write me

## One letter variable names are a no go

write me

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
