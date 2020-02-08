# Regex

This repository implements a simple lexical analyzer, parser, and code generator for a simplified Regular Expression language.

# Lexical Analyzer

## Parsers

There are two parsers planned for this compiler. The first will use a simple recursive descent parsing algorithm, while the second will use some bottom-up parsing algorithm. The following are the planned features for both parsers:

- [ ] Complete parsing of the grammar given below (see following subsection).
- [ ] Perform error recovery and continue parsing where possible.
- [ ] Parse common syntax errors and provide friendly error messages.

### Grammar


```
[X] Program         -->         Expression EOF
[X]                 |           ^ Expression EOF
[X]                 |           Expression $ EOF
[X]                 |           ^ Expression $ EOF
[X] Expression      -->         Concat Expression'
[X] Expression'     -->         ""
[X]                 |           | Expression
[X] Concat          -->         Operand Concat'
[X] Concat'         -->         ""
[X]                 |           Concat
[X] Operand         -->         ( Expression ) Mark             // Operand is anything that could 
                                                            // be an operand on
                                                            // the immediate left of an operator.
[X]                 |           Range Mark
[X]                 |           Character Mark
[X] Mark            -->         ""
[X]                 |           *                           // *, ?, and + bind the tightest
[X]                 |           ?
[X]                 |           +
[X] Range           -->         [  RExpr ]
[X]                 |           [ ^RExpr ]
[X] RExpr           -->         Character RExpr'
[X] RExpr'          -->         ""
[X]                 |           - Character RExpr''
[X]                 |           RExpr
[X] RExpr''         -->         ""
[X]                 |           RExpr                       // This is not left-recursive, since to get here
                                                            // from RExpr, we are 
                                                            // forced to consume input
[X] Character       -->         *Any terminal character that does not have 
                                special meaning in the context of the Regex Engine
```

## Properties of the Abstract Syntax Tree

After parsing, the code generator will traverse the new AST and generate an NFA that recognizes the language represented by the input regular expression. To do so, the code generator expects the AST to have the following features:

```
[X] No "Grouping" tokens as the head of subtrees -- that is, the AST is as flat as possible
[X] No unnecessary nodes with only one child; i.e. a node that represents an expression, with one child, the Union operator. In this case, the Expression node adds no value to code generation. An example of a necessary one-child node is a Range node with one child, a Character node. In this case, the Range node indicates that its children are to be treated differently from normal nodes, since they are part of a Range construct.
[X] No nodes that duplicate grouping features inherent in a tree; i.e. nodes that represent parentheses
```

If the AST doesn't have these features, the code generator will fail.

## Code Generation of Finite State Automata

For a given node of the AST with a token, an NFA will be generated for the following token types, by incorporating the children nodes:

```
[X] Character (outside a range or range complement)
[X] Escaped Character (outside a range or range complement)
[X] Union
[X] Concat
[X] Wildcard
[X] Plus
[X] Optional
[X] Range
[X] Range Complement
```

The following token types are used to help generate an NFA, but in and of themselves do not warrant an NFA:

```
[X] Range Interval
[X] Character (within a range or range complement)
[ ] Escaped Character (within a range or range complement)
```

The following token types are used to help direct the operation of the Regex Engine, but are not part of the NFA and should be ignored if encountered:

```
[X] Match Start
[X] Match End
```

## Appendix

### Draft Grammar

The context-free grammar the parser is based on the following draft:

```
Program         -->         Expression EOF  
Expression      -->         Concat  
                |           Union  
                |           Range  
                |           Character
                |           ( Expression )  
Concat          -->         Expression Expression  
Union           -->         Expression | Expression  
Range           -->         [ RExpr ]  
                |           [ ^RExpr ]  
RExpr           -->         Character RExpr  
                |           Character  
                |           Character - Character RExpr  
                |           Character - Character  
Character       -->         *Any terminal character that does not have special meaning in the context of the Regex Engine  
```

So what's the problem with the draft? The problem is that it suffers from

- Left recursion (Expression --> Concat --> Expression)
- Not every production is left-factored
- precedence and associativity are not clearly expressed

### Draft #2

```
Program         -->         Expression EOF
Expression      -->         ( Expression )              // Here Expression represents all possible expressions
                |           Concat | Expression         // An Expression is a Union of a Concatenated prefix and and Expression.
                                                        //      This effectively chooses the first union if it is present
                |           Concat                      // Or there is no Union at all.
Concat          -->         ( Concat )         
                |           Unary Concat
                |           Unary                       // Unary matches based on a single set of characters
Unary           -->         ( Unary )
                |           Range
                |           Character *
                |           Character ?
                |           Character +
                |           Character
Range           -->         [ RExpr ]
                |           [ ^RExpr ]
RExpr           -->         Character RExpr'            // Here we've managed to do some left-factoring
RExpr'          -->         ""                          
                |           - Character RExpr'
                |           Character RExpr'
Character       -->         *Any terminal character that does not have special meaning in the context of the Regex Engine
```

So what's the problem with draft #2? We've managed to make the precedence clear (unary > concat > union), and both concat and union are bi-associative operations, so there's no issue there. We've also managed to remove left-recursion in most cases. But there are still a couple issues:

- Not every production is left-factored


### Draft #3

```
Program         -->         Expression EOF
Expression      -->         ( Expression )
                |           Concat Expression'
Expression'     -->         ""
                |           | Expression
Concat          -->         ( Concat )
                |           Unary Concat'
Concat'         -->         ""
                |           Concat
Unary           -->         ( Unary )
                |           Range
                |           Character Mark
Mark            -->         ""
                |           *                           // *, ?, and + bind the tightest
                |           ?
                |           +
Range           -->         [ RExpr ]
                |           [ ^RExpr ]
RExpr           -->         Character RExpr'
RExpr'          -->         ""
                |           - Character RExpr''
                            RExpr
RExpr''         -->         ""
                |           RExpr                       // This is not left-recursive, since to get here from RExpr, we are 
                                                        //      forced to consume input
Character       -->         *Any terminal character that does not have special meaning in the context of the Regex Engine
```
