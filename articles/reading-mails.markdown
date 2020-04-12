---
title: Reading mails
date: 2019-08-29
info: Reading a lot of emails is not trivial. Here are are my experiences which might help you with your setup.
draft: true
---

# About

I used Thunderbird a very long time, and it was quite a pain.
The more I did with other people the more mails I got which
I was couldn't oversee anymore.
So it was time for a change.
Friends told me use isync and notmuch.
Which I did, and now I'm quite happy with the setup.
But it took a while to figure out what setup the creators
had in mind, when they created theses tools.

# Overview

Here is an overview of the information flow, and where stuff is stored.


![Showing the flow of information and which information is stored where.](../images/notmuch-overview.svg)

## Tools

These are the tools I'm using in my setup.

* [mbsync/isync](http://isync.sourceforge.net/mbsync.html) : 
syncs your ~/Maildir with your Mail servers via IMAP.
* [notmuch](https://notmuchmail.org/) :
a tag database, which is very fast to 
* [msmtp](https://marlam.de/msmtp/documentation/) : 
sendmail like program to send your  emails through using your 
* [alot](https://github.com/pazz/alot) :
command line interface for notmuch, capable of writing and reading mails.
* [afew](https://github.com/afewmail/afew) :
A tool to automatically tag your mails using notmuch
#### text ####
text:

# using IMAP as point of truth

write me

# alot

Here are some topics on how to use alot.

## Showing Images

write me

## Encryption

write me

## Attache content

write me
