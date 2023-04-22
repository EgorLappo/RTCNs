# RTCNs


Writing math-related code for trees is easy: you define them as a recursive type, e.g.

```
data Tree a = Leaf a | Node a [Tree a]
```

and many functions (finding children, descendant leaves, MRCA, etc.) basically write themselves.

Phylogenetic networks are not recursive structures anymore, so its much harder to work with them. This repo contains some utility functionality for working with rooted phylogenetic networks (RPNs) from a mathematical point of view. 

I encode RPNs essentially as adjacency lists, which makes some functions harder to write, and others less efficient (at least in my implementation). 