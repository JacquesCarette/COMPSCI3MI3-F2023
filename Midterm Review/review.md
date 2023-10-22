# Midterm Review Tutorial

## BNF Grammars

Backus-Naur Form (BNF) grammars are a 'DSL'/formal syntax for defining
context-free grammars (CFG), specifically for CFGs as designed by Chomsky. They
declare the structure of terms of a language.

BNF grammars have 3 key components: terminals, non-terminals, and production
rules.

### Example: Mathematical Expression Languages

_Adapted from <https://www.geeksforgeeks.org/bnf-notation-in-compiler-design/>_


**BNF**:
```
expr ::= term '+' expr
       | term
term ::= factor '*' term
       | factor
factor ::= natural 
         | '(' expr ')'
natural ::= ('1' | ... | '9') natural
          | ('0' | ... | '9')
```

Let's find 1+2*3

```
expr
=> term '+' expr
=> factor '+' expr
=> natural '+' expr
=> '1' '+' expr
=> '1' '+' term
=> '1' '+' factor '*' term
=> '1' '+' natural '*' term
=> '1' '+' '2' '*' term
=> '1' '+' '2' '*' factor
=> '1' '+' '2' '*' natural
=> '1' '+' '2' '*' '3'
=> 1+2*3
```

## Prolog, Haskell, and the land of programming languages

* How are these two languages different? How are they the same?
* Let's look at some simple examples!
* What issues could we run into when translating code?

## Embedding Domain-Specific Languages

We've shown 3 styles in lectures: _shallow_, _deep_, and _tagless_, specifically
in the context of Haskell.

### Shallow

* Restricted to 1 interpretation of an AST (fixed semantics). ASTs are not
  reusable!
* Easily extend AST terms by defining new functions.

### Deep

* ASTs are reusable! Can define multiple interpretations for an AST.
* Typically, can't extend the language easily without excessive recompilation
  and forced to define interpretation of new terms for all existing
  interpretations (not always desired!).

### Tagless

* Extensible syntax.
* Extensible interpretations.

## Evaluation Strategies

* Big question: how do we evaluate function calls? Lots of different options here!
* Use the Lambda Calculus as a simple model of languages with functions
* Consider a term like
  $$
  (\lambda x. \lambda y. x x y) ((\lambda x. x) a) b
  $$
  
  Multiple places we can reduce: which ones do we pick?
* Furthermore, when do we decide to stop evaluating?
  Expressions that cannot be evaluated further with a given
  reduction strategy are called **normal forms** for that
  strategy.
  
### Full Beta-Reduction
Pick any arbitrary redex in any order: non-deterministic.

### Normal Order
Pick the left-most, outermost redex first, and keep
going until there are no more redexes.

### Call-By-Value
Evaluate arguments to function calls before evaluating the function call.
Stop when the outermost term is a lambda. This is the evaluation strategy
most languages adopt.

```
(λ x. λ y. x x y) ((λ x. x) a) b
(λ x. λ y. x x y) a b
(λ y. a a y) b
a a b
```

### Call-By-Name
Evaluate function calls without evaluating arguments.
Stop when the outermost term is a lambda.

```
(λ x. λ y. x x y) ((λ x. x) a) b
(λ y. ((λ x. x) a) ((λ x. x) a) y)  b
((λ x. x) a) ((λ x. x) a) b
a ((λ x. x) a) b
a a b
```

### Call-By-Need
Note that Call-By-Name duplicated work: every time we used $x$ in the body, we
had to re-compute the argument. Call-By-Need solves by sharing work, and uses syntax
_graphs_, not syntax trees. This is the reduction strategy used by Haskell.

### Further Strategies

This isn't an exhaustive list: there are many more choices! However, we do
not expect you to know what these other choices are.
