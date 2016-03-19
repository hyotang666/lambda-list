# [Function] LAMBDA-LIST-KEYWORD-P

## Syntax:

(LAMBDA-LIST-KEYWORD-P SYMBOL &optional (ansi t)) => BOOLEAN

## Arguments and Values:

ansi := BOOLEAN

## Description:

If SYMBOL is lambda list keyword, evaluated to be T.

When optional parameter ANSI is specified NIL, if SYMBOL-NAME's first character is #\&, evaluated to be T.
This facility is useful when you customize lambda list keyword.
(You can find such usecase in system ESRAP, for example.)

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:

