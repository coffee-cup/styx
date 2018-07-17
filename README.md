# styx

Styx is an interpreted (eventually compiled) programming language.

## Features

- Functional
- Impure
- Strongly typed
- Type inferred
- Abstract data types
- Function currying
- Psudeo-assignment (converted interally to lambda application)

_This is a very large work in progress and does not currently work :D I will be committing a lot of time to this after this semester._

## Syntax

When complete, the syntax will look something like

```haskell
data Maybe a = Nothing | Just a

data Person = Person
  { name :: String
  , age  :: Maybe Int
  }
  
sayHello :: Person -> ()
sayHello Person n (Just a) = 
  print ("Hello " ++ n ++ "! You are " ++ (show a))
sayHello Person n Nothing = 
  print ("Hello " ++ n)

main :: ()
main =
  jim = Person "Jim" (Just 20)
  sam = Person "Sam" Nothing
  sayHello jim
  sayHello sam
  
-- Hello Jim! You are 20
-- Hello Sam
```

## A Note on Assignments

Variable assignment is not normally a feature of functional languages. However, I wanted styx to be more accessible and familar to those coming from an imperative background. Under the hood assignment is treated like function application to a lambda.

```haskell
x = 1
print x

```

is equivalent to

```haskell
(\x -> print x) 1
```

This way all assignment can be type inferred properly. This also means that _assignment_ is allowed. This does not mean that variables are mutable in a traditional sense, because this

```haskell
x = 1
x = 2
print x
```

is translated to


```haskell
(\x ->
  (\x ->
    print x
  ) 2
) 1
```

The binding x is shadowed in the inner lambda so the value 1 is never used.

After this language is complete, I may come to regret this design decision (I expect compiler optimizations to be difficult). But right now, it is an idea I am trying out with this language.

## TODO

- [x] Repl
- [x] Parser
- [ ] Type checking and inference
- [ ] Interpreter
- [ ] Code generation
