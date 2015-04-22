# Containers

The containers package is the home of high performance *immutable*
datastructures. The implementation and interface for the different structures
are all conducive to a highly functional style of programming. You will likely
find that you are will be quite comfortable using these data structures based on
basic Haskell concepts. The purpose of this document is to give you the
intuition behind use cases, and to provide enough examples for a programmer to
make use of these fast data structures. 

The `List` type is the first illustration of a functional datastructure you have likely
encountered. Recall its definition

```
List a = Empty
       | Cons a (List a)

```

and the standard functions that operate on it: maps, folds, scans, etc. Now,
consider that the recursive definition of lists is what makes them pure and
immultable. Lists are not like their imperative counterpart Arrays, which
represent blocks of memory that can be mutated. Lists are important to
understand in learning the specialized datastructures in the containers library,
and all of the techniques that you have learned in dealing with them will
directly apply to making the best use of them. 

In a language like Haskell, immutable datastructures should be prefered to
mutable ones. Unless, there is another reason for choosing a mutable
datastructure. There are many such valid reasons, mostly due to the fact that a
large number of algorithms are better written with a mutable structure. 

Lists are of course slow at many operations, and they don't make sense from a
performance perspective outside their special usecases. It's fine to favor lists 
in production code in the following areas:

* Operations that act on each element in order
* Situations where you don't care about performance

# Usecases

* Fast insertion, fast lookup
  (Map, IntMap, Set, IntSet)

* Fast prepend and append, fast breaking and concatenation, merging
  (Sequence)

* Many to many relationships
  (Graph)

* Key Value Pairs
  (Map, IntMap)

* 

# Importing

Due to name clashes, importing the set of modules `Set`, `IntSet`, `Map`,
`IntMap` and `Sequence` is done qualified such as:

```
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
...
```

# Common typeclass instances

Foldable containers can be reduced to a single value. Standard folds are
included as functions in the relevent modules, eg. `Data.Map.foldr`. Do not
forget that you also have at your command the functions availible in the
Data.Foldable module. For example mapping to a monandic action `mapM`,
specialized folds, and other common tasks. Some functionalithat you also have at
your command the functions availible in the Data.Foldable module. For example
executing a monandic action `mapM_` on the collection and discarding any
results, specialized folds. For example, the function to lookup if an element is
contained in the values of an `Map` is `elem` from `Foldable`.

```
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

Only the two `Map` types and `Sequence` have valid instances of traversable
written. 

```
let s = fromList [(1,"ab"), (2,"xy")]
> sequenceA s
[fromList [(1,'a'),(2,'x')],fromList [(1,'a'),(2,'y')],fromList [(1,'b'),(2,'x')],fromList [(1,'b'),(2,'y')]]
```

For a more complete introduction to common typeclasses see [the section on
common type classes](../content/common-typeclasses.md)

# Graph

The graph datastructure seems to be the odd structure out, as it does not at
first glance have any of the common functional operations of the other
structures we have been describing. However such functionality is hiding beneath
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

Algorithms in the graph package derive from bredth first searches, such as
determing if it is possible to reach one node to another node through a set of
connected edges, or finding strongly connected components (subgraphs where the
nodes are all reachable to each other). If you are looking for doing numerical
work on weighted graphs you may want to use a different package such as
`hmatrix`. 

# Sequence

Sequences are a high performance list-like data structure that first
may look like double ended lists, but end up being much more powerful than that.

