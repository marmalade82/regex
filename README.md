# Regex

This repository implements a simple lexical analyzer, parser, and code generator for a simplified Regular Expression language.

## Grammar

```
Program         -->         Expression EOF
Expression      -->         ( Expression )
                |           Concat Expression'
Expression'     -->         ""                          // An empty production helps. It means that if all other clauses fail, 
                                                        //      this optional production
                                                        //      still succeeds. It does NOT mean that we match on empty input.
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
                |           Character RExpr'''          
RExpr''         -->         ""
                |           RExpr                       // This is not left-recursive, since to get here from RExpr, we are forced to consume input
RExpr'''        -->         ""
                |           RExpr
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
```
