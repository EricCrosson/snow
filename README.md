# Snow ![Build status](https://travis-ci.org/EricCrosson/snow.svg) [![GNU GPL](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html) [![Coverage Status](https://coveralls.io/repos/EricCrosson/snow/badge.svg?branch=master&service=github)](https://coveralls.io/github/EricCrosson/snow?branch=master)

## Installation 

Snow simulation

Install this package with [quelpa](https://github.com/quelpa/quelpa) by
evaluating the following code

```lisp
(quelpa '(snow :repo "EricCrosson/snow" :fetcher github)))
```

Run this package by invoking the autoloaded command `M-x snow`.

## Improvements 

The original idea for this package came from a bash script, seen on
[StackOverflow](http://stackoverflow.com/questions/8608226/simulate-bash-snow-fall-script-in-ncurses-c). One
day I found it remarkably strange that I only opened a terminal when I wanted to
watch the snow fall, and decided to implement snow natively inside my favorite
computing environment.

Along the way, the feature list has grown. This version of snow includes:
- documentation
- debug mode
- timestep counter
- storms generated by random or user-entered seeds

## Thanks

Thanks to Kyle Jones for writing `life.el`, which I found very helpful to browse.
