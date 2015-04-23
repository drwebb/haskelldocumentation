# Containers

The containers package is the home of high performance *immutable* data
structures. The implementation and interface for the different structures are
all conducive to a highly functional style of programming. If you are new to
this library, you will already be quite comfortable using these data structures
based on basic Haskell concepts you have learned. This may seem foreign at
first, but the purpose of this document is to give you the intuition behind use
cases and to give some concrete examples.

The `List` type is the first illustration of a immutable data structure you have
likely encountered. Recall its definition:

```
List a = Empty
       | Cons a (List a)

```

and the standard functions that operate on it: `map`, `fold`, `scan`, etc.
Consider the fact that it is the recursive definition of the `List` type is what
makes them pure and immutable. If this sounds new, you can just think about how
any list is defined as a series of function applications to the `Empty` list,
with the data constructor `Cons` being that function. Lists are not like their
counterparts in imperative languages such as C arrays, which represent blocks of
memory that can be mutated. Lists are important to understand in learning the
specialized data structures in the containers library, and all of the techniques
that you have learned in dealing with Lists will directly apply to making the
best use of the new structures. 

In Haskell, immutable data structures should be preferred to mutable ones.
Unless, there is another reason for choosing a mutable data structure. There are
many such valid reasons.  Some algorithms on mutable structures may be more
clear to write or faster, but with immutable structures we can more easily
reason about issues such as thread safety. 

Lists are of course slow at many operations (indexing, appending), and in many
common use cases they won't work well from a performance perspective in your
applications. However, you will find that in practice there are comparable
number of use cases where lists already perform optimally. It's fine to favor
lists in production code in the following areas:

* Operations that act on each element in order
* Stack or Stream like data structures.

When it is clear that your program is bottlenecking will bottle neck with a List, 
consider the usecases below in choosing a new data structure. Also, familiarize
yourself with the basic typeclasses in Haskell, so you can refactor your code
faster to the better structures. Also recognize that the tools available
to the Haskell programmer such as pattern matching and recursion work equally
the same when dealing with these functional data structures. With these concepts
in mind you will be on the right road to clear high performance Haskell code. 

# Usecases

* Fast insertion, fast lookup
  (Map, IntMap, Set, IntSet)

* Fast prepend and append, fast breaking and concatenation
  (Sequence)

* Key Value Pairs
  (Map, IntMap)

* Distinct Objects
  (Set, IntSet)

* Many to many relationships
  (Graph)
 
* Hierarchical/Branching Data
  (Tree)

## When not to use containers 


# Importing

Due to name clashes, importing the set of modules `Set`, `IntSet`, `Map`,
`IntMap` and `Sequence` is done qualified such as:

```
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
...
```

# Common typeclass instances

Now we will discuss the importance of typeclasses in relation to containers.
Consider for instance `Foldable` containers, which means the container can be
reduced to a single value. Standard folds are included as functions in the
relevant modules, eg. `Data.Map.foldr`. Do not forget that you also have at your
command the functions available in the Data.Foldable module. For example
`mapM_`, specialized folds, and other common tasks are all found in this module.
It may appear that a container is missing a basic operation, when it is really
hidden as a function on the typeclass.  For example, the function to lookup if
an element is contained in the values of an `Map` is `elem` from `Foldable`,
while the `Data.Map` module only contains the function `member` to test if a key
is present.

```
> import Data.Foldable
> let ngons = [("Square", 4), ("Pentagon", 5), ("Hexagon", 6)] :: Map String Int
> mapM_ print ngons
4
5
6
> :t flip elem ngons 
flip elem ngons :: Int -> Bool
> :t flip member ngons
flip member ngons :: String -> Bool

```

`Map` and `Sequence` have instances of traversable
defined:

```
let s = fromList [(1,"ab"), (2,"xy")] :: Map Int String
> sequenceA s
[fromList [(1,'a'),(2,'x')],fromList [(1,'a'),(2,'y')],fromList [(1,'b'),(2,'x')],fromList [(1,'b'),(2,'y')]]
```

For a more complete introduction to common typeclasses see [the section on
common type classes](../content/common-typeclasses.md)

(Maybe some sort of matrix?) 

(Do I really need examples here)

(Explain GHC 7.10 changes moving to Prelude)

(Mention classy prelude)

# Sequence

Sequences are a high performance list-like data structure that first
may look like double ended lists. They are very powerful and useful.

Show examples for pattern matching (and view patterns extension).

Show an example for an algorithm,

# Map and IntMap

Show lookups, handling Maybe return type.

Show monoid (i.e. union) code.

Discuss int map performance (need reference)

# Set and IntSet

# Tree

(Need real world examples, maybe ASTs, or hierarchical data)

(Comonad instance? Zippers?)

# Graph

The graph data structure seems to be the odd structure out, as it does not at
first glance have any of the common functional operations of the other
structures we have been describing. It is also a structure that is designed to
help model relationships.

However such functionality is hiding beneath
the surface if we examine the types. A graph is object originally from
mathematics which consists of Vertices and Edges that connect the verticies.
They can be used to model such exciting areas such as the electrical power
station grid, DNA signally pathways, and neural systems.


We can specify graphs from arbitrary vertex types using `graphFromEdges` or a by
using the more direct `buildG` which just uses a list of edges.
``` 
>:t graphFromEdges
graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph, Vertex ->
  (node, key, [key]), key -> Maybe Vertex) 
>let (g, f, k) = graphFromEdges [("NYC", 0, [1,2]), ("London", 1,[2]),
   ("Paris",2,[0,1,3]), ("Frankfurt", 3, [1,2]), ("Saigon", 4, [5]), 
   ("Moscow", 5, [])]
array (0,5) [(0,[1,2]),(1,[2]),(2,[0,1,3]),(3,[1,2]),(4,[5]),(5,[])]
> let loveTriangle =  buildG (0,2) [(0,2),(1,2),(2,1)]
array (0,2) [(0,[2]),(1,[2]),(2,[1])]
```

(How to use a graph to access data, convert to list?)

Algorithms in the graph package derive from bredth first searches, such as
determing if it is possible to reach one node to another node through a set of
connected edges, or finding strongly connected components (subgraphs where the
nodes are all reachable to each other). If you are looking for doing numerical
work on weighted graphs you may want to use a different package such as
`hmatrix`. 

