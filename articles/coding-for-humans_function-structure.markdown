---
title: "Coding for Humans : Function Structure"
date: 2020-03-13
info: Coding is hard, it seems. I frequently face the same problems. 
draft: true
---

Coding is hard,
well it seems so, because I (and others) have usually a hard time to read code from others.
This is not always the case of course. But it seems that over the time readable code
from different people appears to me readable because of the same aspects.
In my experience readability is not related to specific languages.

<!-- link zu dem typen der gesagt hat das jeder programmieren kann nur programmieren können so programmieren das andere es verstehen -->


# Functions Queue and Functions calling functions

* write modular code.
* fail fast
* do not queue functions, fall them.

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
