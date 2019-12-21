---
title: Cheatsheet BASH
date: 2020-11-11
draft: true
info: |
  Bash is one of the most important tools out there
  to maintain Operation Systems.
---

# How to process Lines containing Spaces

If you want to process all files in a folder, you most likely run into the problem of filenames containing spaces.

```bash
find . -type f | while read line; do echo $line ; done
```

will do the trick.
