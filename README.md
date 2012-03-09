# mgm7734.util

Misc. utility functions and macros, such as:

	condl          cond with :let clauses
	count-bound    compare length of possibly infinite seq: (count-bound 3 < (repeat :x)) => true
	match-case     pattern matching w richer syntax but slower than clojure.match
	reducer        reduce right-to-left [preserves laziness]

## Usage

(use 'mgm7734.util)


## License

Copyright (C) 2012 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
