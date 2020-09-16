---
name: Style Request
about: Suggest an idea for code styling
title: ''
labels: configuration idea, style change
assignees: ''

---

Currently, `$formatter` (the formatter you want to change) formats this code…

```erlang
some:code(you, want, to, format).
```

…like this…

```erlang
case current:version() of
  the -> formatted:code();
```

I would like it to be formatted like this…

```erlang
case expected:version() of
  the ->
    formatted:code();
```
