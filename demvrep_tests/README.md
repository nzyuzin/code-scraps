Test files
==========

Test file format
----------------

```
test-file ::= list-of-nodes new-line list-of-edges new-line
list-of-nodes ::= empty-string | node | node newline list-of-nodes
node ::= d | r
list-of-edges ::= empty-string | edge | edge newline list-of-edges
edge ::= integer integer
```
