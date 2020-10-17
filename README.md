# Miscellaneous Haskell Benchmarks

This is a repository to store various benchmarks of Haskell data structures
and algorithms. Generate the report by typing into your terminal:

```bash
$ sh generate-report.sh
```

Currently, we have strings vs text and strict maps vs hashmaps, as these
are the data structures I most often have to choose between. For almost
everything, text is faster than string, which was to be expected. On the
other hand, hashmaps have slower updates than maps but much faster lookups,
so what you choose there is dependent on the situation. Consuming the entirety
of a map via `foldl'` or `foldr` is also much faster for maps rather than
hashmaps, so if you're going to be doing a lot of that it makes sense to
keep things in a map.

The bench.html currently in this repository was generated with GHC 8.10.
