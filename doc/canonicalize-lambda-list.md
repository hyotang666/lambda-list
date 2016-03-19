# [Function] CANONICALIZE-LAMBDA-LIST

## Syntax:

(CANONICALIZE-LAMBDA-LIST lambda-list) => lambda list

## Arguments and Values:

lambda-list := Ordinary lambda list.

## Description:

Canonicalize dotted lambda list with &REST lambda list keyword.

## Example:
```lisp
(canonicalize-lambda-list '((a . b) . c)
=> ((A &REST B) &REST C)
```
## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:

