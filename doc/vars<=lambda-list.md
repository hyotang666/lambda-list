# [Function] VARS<=LAMBDA-LIST

## Syntax:

(VARS<=LAMBDA-LIST lambda-list &key (as :function)) => vars

## Arguments and Values:

lambda-list := ordinary lambda list

as := (MEMBER :FUNCTION :MACRO :METHOD)

vars := (SYMBOL\*)

## Description:

Collect vars from lambda list.

## Example:
```lisp
(vars<=lambda-list '(a &key(b :init)))
=> (A B)
```
## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:

Depends on context, sometime destructuring is invalid.
In such case an error will be signaled.
You can controll context by specifying keyword parameter AS.
