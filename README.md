[![CI](https://github.com/alessandrocandolini/categorical-parsing/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/categorical-parsing/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/categorical-parsing/branch/main/graph/badge.svg?token=0yn0y8khDX)](https://codecov.io/gh/alessandrocandolini/categorical-parsing)

# categorical-parsing

Category theory is a branch of mathematics concerned with the study of properties of mathematical structures and their relationships. A key aspect is that category theory achieve this by neglecting all information about the structure of the mathematical entities (ie, objects), and focusing entirely on the relationships between them (ie, arrows). 

This might seem surprising at a first sight, but it's one of the fascinating aspects of category theory that it's possible to explore, investigate, analyse, study, and ultimately define mathematical entities by forgetting entirely about their structure and focusing exclusively on their relationships. 

This way of thinking seems particularly appealing in other areas of applied mathematics (eg, physics or computer science). In computer science for example, category theory ultimately allows us to define programs by their behavioural and compositional properties first, before worring too much about how to encode this in a concrete structure. In fact, as consumers of a program, we rarely care about internal implementation details, and instead we want to understand how to safely manipulate programs in terms of other programs, through "combinators" that have a well-defined behaviour (eg, preserve certain properties, satisfy precise laws, etc). 

This way of thinking will be illustrated here in building from scratch a small library of "monadic parser combinators", ie, an approach to parsing where the parser is indirectly defined by it's behaviour. The code shows how to create combinators that allow to create new parsers from existing ones. We will see in particular that after defining a single basic parser capable of parsing any char, we can build from scratch (and in few lines of haskell code) much more complex parsers (eg, json parsing, or parser of a simple arithmetic expression). 

A cornestone paper in this area is the one from [Hutton and Mejier (1996)](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf), culminating in the early development of [parsec]( https://hackage.haskell.org/package/parsec), an industrial-strength parser combinator library which later has inspired the development of other forks in the Haskell ecosystem, notably [attoparsec](https://hackage.haskell.org/package/attoparsec) and [megaparsec](https://hackage.haskell.org/package/megaparsec). The paper is highly recommended. 

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```
To run with test coverage
```
stack test --coverage
```
which generates a textual and HTML report.

To run the executable,
```
stack exec categorical-parsing-exe -- -e "(3*(1+2))" 
> Just 3

stack exec categorical-parsing-exe -- -e "(3(1+2))" 
> Nothing 
```

For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.


## Future directions

Would be nice to have time to expand the project to cover some or all of the following:
* comprensive algebra of core combinators
* a number of additional examples (json parsing, yaml parsing, etc) 
* applicative parsing 
* pretty printing, in the spirit of https://belle.sourceforge.net/doc/hughes95design.pdf
