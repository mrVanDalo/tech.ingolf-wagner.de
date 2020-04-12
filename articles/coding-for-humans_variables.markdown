---
title: "Coding for Humans : naming things"
date: 2020-01-13
info: Coding is hard, it seems. I frequently face the same problems. 
draft: true
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
which developers have to understand to make larger changes.

Keeping the context small is the art.
There are various methods on how to do that, let's talk about naming things.

## Naming things

Proper variable and function names make it easy for people to read your code.
Because it reduces the context they have to carry.

So let's talk about what are not proper names,
on letter variables short forms or letters left out words are
in general no proper names.

For examples:

`ctx`, `req`, `res`, `bdy`, `buf`, ... 

might seem to be familiar and kind of a standard, but reading something
like 

`context`, `request`, `result`, `body`, `buffer`, ...

does not leave any questions open.

### Examples

let's look at some examples.

<!-- hard example : https://pypi.org/project/pycairo/ -->

This is some piece of code that uses curl to print a page in python.

```python
buffer = BytesIO()
curl = pycurl.Curl()
curl.set_option(curl.URL, 'http://pycurl.io/')
curl.set_option(curl.WRITEDATA, buffer)
curl.perform()
curl.close()
body = buffer.get_value()
print(body.decode('iso-8859-1'))
```

sadly this is not how the example and the curl library works.
Here's the real example (from https://github.com/pycurl/pycurl/blob/master/examples/quickstart/get.py).

```python
buffer = BytesIO()
c = pycurl.Curl()
c.setopt(c.URL, 'http://pycurl.io/')
c.setopt(c.WRITEDATA, buffer)
c.perform()
c.close()
body = buffer.getvalue()
print(body.decode('iso-8859-1'))
```

the author decided to create a function called `setopt` instead of writing 4 more letters 
and call it `set_option`. Here is an even shorter version
where function calls and 

```python
bf = BytesIO()
c = pycurl.Curl()
c.setopt(c.URL, 'http://pycurl.io/')
c.setopt(c.WRITEDATA, bf)
c.perf()
c.close()
b = bf.getval()
prt(b.dec('iso-8859-1'))
```

Reading and understanding this snippet takes significantly longer than the first snippet.
Because every word needs to be decoded (by your brain),
this decoding process needs the context to work.
This is a very small context, but the bigger the context becomes,
the more time you spend decoding the function and variable names,
instead of understanding what the function actually does.

And don't fool yourself, it is not easier to use theses short letters
while you are writing the code. You have to decode them,
and sort them into your context.

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


# Use variables to increase readability

Readability is key. One-Liners are not an Art-form, even if you use comments to describe what you are doing.

Example, lambdas in haskell.
