# Exemplar 
[![CircleCI](https://circleci.com/gh/CoNarrative/exemplar.svg?style=shield)](https://circleci.com/gh/CoNarrative/exemplar)
[![codecov](https://codecov.io/gh/CoNarrative/exemplar/branch/master/graph/badge.svg)](https://codecov.io/gh/CoNarrative/exemplar)
[![Clojars Project](https://img.shields.io/clojars/v/com.conarrative/exemplar.svg)](https://clojars.org/com.conarrative/exemplar) 


Record functions.

> Show me what you got. I want to see what you got.
> - Teddy Roosevelt

## Fake data
Things like type systems allow us to describe data in general terms. But what happens when we need 
instances of those descriptions and we have none? Typically we make something up so we can 
continue writing software until the real data arrives. We use fake data. 
Despite the importance it plays in getting software built, it's not something we talk about much.

How do we get fake data? Sometimes we write it out by hand. 
Other times we write code that's sole purpose is to generate it.
We copy and paste the result of database queries or API calls. We 
put the world into particular state, log whatever it is we're looking for, and copy and paste
 it into our workspace. 
  

However we do it, we spend significant time and effort obtaining and working with fake data. 
Yet, unless it makes it into a test, we throw it away. 

What if we didn't do that?

```clj
(ns my-ns
  (:require [exemplar.core :refer :all])
 
(spit "my-file.edn" {})
(register-path "my-file.edn")

(defn my-function [a b c] (+ a b c))

(save (my-function 1 2 3))
=> 6

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}

(run my-function)
=> 6
```

## Why keep fake data?
It can be nice to see an example of the input a function was designed to receive. This can 
be especially helpful in cases where the input to a function is not defined by a spec.
If obtaining fake data is something we'd like to do less of, saving it might prevent us from 
recreating it later. Even when we change the input a function receives, having an example of the 
old input might be helpful. We might be able to slightly modify the old example input to produce 
the new.

What else could we do with persisted mock data? We can generate 
tests. For pure functions, this is especially trivial. 

We can generate documentation that includes example inputs and outputs.

And if you're already using fake data in development, this should all be free.

## Recording a running application
Exemplar can record the same information about a function as an application is running. 
Functions can be recorded once or until an explicit call to `stop-recording`. Persisted 
entries for functions are overwritten. The data persisted is always that 
of the last call, so you won't accumulate 1,000 example inputs and outputs per function. 

Record once:
```clj
(ns my-ns
  (:require [exemplar.core :refer :all])
 
(spit "my-file.edn" {})
(register-path "my-file.edn")

(defn my-function [a b c] (+ a b c))

(record-once my-function)

(my-function 1 2 3)
=> 6

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}

(my-function 2 3 4)
=> 9

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}
```
Record until stop:
```clj
(ns my-ns
  (:require [exemplar.core :refer :all])
 
(spit "my-file.edn" {})
(register-path "my-file.edn")

(defn my-function [a b c] (+ a b c))

(record my-function)

(my-function 1 2 3)
=> 6

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}

(my-function 2 3 4)
=> 9

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [2 3 4]
    :out 9
    :source "(defn my-function [a b c] (+ a b c))"}
    
(stop-recording my-function)

(my-function 1 2 3)
=> 6

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [2 3 4]
    :out 9
    :source "(defn my-function [a b c] (+ a b c))"}
```


For convenience, you can also record entire namespaces.

```clj
(ns my-ns
  (:require [exemplar.core :refer :all])
 
(spit "my-file.edn" {})
(register-path "my-file.edn")

(defn my-function [a b c] (+ a b c))
(defn my-other-function [xs] (map inc xs))
(def some-def {:cool true)
(defmacro some-macro [sym] `[~sym])

(record-namespace my-ns)

(my-function 1 2 3)
=> 6
(my-other-function [1 2 3])
=> (2 3 4)

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}

(show my-other-function)
=> {:name my-other-function
    :ns my-ns
    :in [[1 2 3]]
    :out (2 3 4)
    :source "(defn my-other-function [xs] (map inc xs))"}
    
(my-function 2 3 4)
=> 9
(my-other-function [2 3 4])
=> (3 4 5)

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}
    
(show my-other-function)
=> {:name my-other-function
    :ns my-ns
    :in [[1 2 3]]
    :out (2 3 4)
    :source "(defn my-other-function [xs] (map inc xs))"}
    
(stop-recording-namespace my-ns)

(my-function 3 4 5)
=> 12
(my-other-function [3 4 5])
=> (4 5 6)

(show my-function)
=> {:name my-function
    :ns my-ns
    :in [1 2 3]
    :out 6
    :source "(defn my-function [a b c] (+ a b c))"}
    
(show my-other-function)
=> {:name my-other-function
    :ns my-ns
    :in [[1 2 3]]
    :out (2 3 4)
    :source "(defn my-other-function [xs] (map inc xs))"}
```

## FAQ
#### You're recording all inputs and outputs to a function?
We do record them but overwrite each call on disk and in memory. So you only get to see the last 
one. We will probably add an option to persist each call but we are only one person at the time 
of this writing.

#### Why doesn't it work from a REPL?
You can use a REPL but the function to be recorded or saved must be written in a file. We 
read from a file as part of saving and recording. 

#### Why is my persisted function definition wrong?
The line numbers in the REPL you're running don't correspond to what's on disk. Restarting the 
REPL should fix it.

#### Does it work with macros?
No. We haven't tried yet. It might be difficult or impossible, but we plan to
 try to support them (we use them a lot). Community input helps us prioritize, so let us know if 
 it's a feature you'd like to see.

#### Does it work with impure functions?
Yes, but there's no magic to how we currently handle them. 
We don't know anything about the atoms or 
vars an impure function might reference. A function that performs a side effect and returns 
`nil` will have that its output. A function that performs a computation with a value that changes over time will have that reflected as its output. If there's interest we can probably do more here. I haven't hit a limit of what's possible with Clojure yet.

## Goals
- Have fun
- Generate tests
- Generate documentation
- Allow accumulation of inputs and outputs instead of overwriting


## Acknowledgements
- [Forth tutorial](https://www.forth.com/starting-forth/1-forth-stacks-dictionary/)
- [Jupyter notebook](http://jupyter.org/)
- [Stuart Halloway](https://twitter.com/stuarthalloway)
- [C#](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [clojure.spec](https://clojure.org/about/spec)
- [Dominic Monroe](https://github.com/SevereOverfl0w/)

## License

Apache 2.0
