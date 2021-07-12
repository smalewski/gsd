# Introduction

GSD is an interpreter for a gradually typed language
with Gradually Structured Data.

It's main features are the following:

1. It can typecheck and evaluate programs with different levels of datatype definitions. 
   From no definitions at all (for dynamic programs) to fully defined static programs,
   and the levels in between those two extremes.
   
2. It works with three different matching strategies: sound, exact and complete.
   
# Installation

There are two ways to install the GSD interpreter:
natively in your machine, or virtually
(using a Vagrant VM or a Docker container).
GSD is written in Haskell, so if you already have
`stack` in your system, using GSD natively may be
the easiest alternative.

There is also a working online interpreter for GSD at [pleiad.cl/gsd](https://pleiad.cl/gsd).

## Building GSD natively

To build the source code you need [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
, a package manager for Haskell.

With stack installed, you only need to clone the
repository and use `stack install`.
The compiled binary should be in `.local/bin/gsd`.

```
$ git clone https://github.com/smalewski/gsd.git
$ cd gsd
$ stack install
```

## Building GSD in a Vagrant VM

In a system with Vagrant, you should be able to provision
a VM from the root of the repository.

```
$ git clone https://github.com/smalewski/gsd.git
$ cd gsd
$ vagrant up

Bringing machine 'default' up with 'virtualbox' provider...
==> default: Importing base box 'generic/debian9'...
==> default: Matching MAC address for NAT networking...
==> default: Checking if box 'generic/debian9' version '3.2.24' is up to date...
==> default: Setting the name of the VM: gsd_default_1626062514969_62237
==> default: Clearing any previously set network interfaces...
==> default: Preparing network interfaces based on configuration...
    default: Adapter 1: nat
... [ a long build ] ...
$ vagrant ssh
```

The `gsd` executable should be in path.
Use `gsd --help` to see the command line interface.

## Building GSD in Docker

To build using Docker just clone the repository and
launch the `build` script.

```
$ git clone https://github.com/smalewski/gsd.git
$ cd gsd
$ ./build.sh
... [ a long build ] ...
$ docker run -p 8001:8001 -it gsd
```

The `gsd` executable should be in path.
Use `gsd --help` to see the command line interface.

# Use

GSD has two modes of use: a command line interpreter or
a web server.

```
$ gsd --help
Interpreter for the GSD language

gsd [COMMAND] ... [OPTIONS]

Common flags:
  -? --help       Display help message
  -V --version    Print version information

gsd [eval] [OPTIONS] FILE
  Evaluate source file

Flags:
     --evaluate   Evaluate the program
  -t --typecheck  Just typecheck
  -n --notrace
  -w --withtrace  Print a trace of the execution

Matching strategies:
  -c --complete   Complete strategy
     --exact      Exact strategy
  -s --sound      Sound strategy
Output formats:
  -p --plain      Plain text
  -l --latex      Latex formated

gsd server [OPTIONS]

  -p --port=NUM   Default port: 8001

```

## Interpreter

To interpret a source file, use the command `eval` followed by
the file's name.
```
$ gsd eval examples/bas-1.gsd
```

By default the interpreter typechecks and evaluates programs.
If the flag `-t` is present the interpreter will only typecheck
the source file, returning the type of the last expression.

Matching strategies can be selected using flags.
The following command evaluates the contents of
`examples/bas-1.gsd` using a **sound** matching strategy.
Matches are **complete** if no flag is provided.
```
$ gsd eval -s examples/bas-1.gsd
```

The interpreter supports two output formats, plain text (the default)
and LaTeX. The LaTeX output uses a pair of macros to dynamically hide or show
evidences and types in the web client.
Static versions of those macros can be found in `macros.tex`.

There are some examples in `examples/`, including
the ones presented in the paper.

## Web server

GSD has a web client that renders
the LaTeX output and has syntax
highlighting.

To use the web client, you must first
start the web server.
```
gsd server
```
This server listens to port 8001 by default.

### Running the web client

The easiest way of running the web client locally is
via a Docker container.

```
# From the root of the repository
$ cd web
$ docker build -t gsd-web .
... [ not that long build ] ...
$ docker run -p 8000:80 gsd-web
```

You can now connect to the web client at `localhost:8000` using a web browser.


# OOPSLA 2021 Artifact Evaluation

## Claims to validate

1. The evolution scenario from section 2 works as expected.
2. Unclassified data interacts seamlessly with regular constructors.
3. Correct trace of execution (Section 4.3).

### Evolution scenario (Section 2)

Files `bas-1.gsd`, `bas-2.gsd`, `bas-3.gsd`, and `bas-4.gsd`
in `examples/` contain the different stages of the program
evolution described in Section 2.

To evaluate each of them you can use the `eval` command, as follows:
```
$ gsd eval examples/bas-1.gsd
<String> "{"Success":{"r":3}}" : ? : ?
```
```
$ gsd examples/bas-2.gsd
<String> "{"Success":{"x":3}}" : String : String
```
```
$ gsd examples/bas-3.gsd
<String> "{"Success":{"x":3}}" : String : String
```
As expected, the above results differ only in their type annotations.
The fourth step follows, adding just a wrapper around the arguments.
```
$ gsd examples/bas-4.gsd
<String> "{"Success":{"x":{"N":{"x":3}}}}" : String : String
```

### Working with unclassified data

you can experiment with the interaction between unclassified data and regular constructors,
we include a simple interactive example:
a lambda calculus interpreter
whose expressions are defined in an open datatype, `examples/lambda.gsd`.
Here is an excerpt:

```haskell
open data Expr = Var {x : ?}
               | Lambda {x : ?, e : Expr}
               | App {e1 : Expr, e2 : Expr}

eval env expr =
  match expr with
    Var x      => lookup x env
    Lambda x e => Clos {x = x, expr = e, env = env}
    App e1 e2  => let v1 = eval env e1
                      v2 = eval env e2
                   in (match v1 with
                        Clos x ex envx => eval (insert envx x v2) ex
                        _              => AppliedNonLambda {expr = v1})
```

Try adding new features to the interpreter, such as `Pairs`, without
modifying the datatype definition. 
A simple solution can be found in `examples/pairs.gsd`.

### Trace of execution (Section 4.3)

The evaluation examples can be found in `examples/evaluation.gsd`.
To get a trace of execution, you should give the `-w` flag to
the `eval` command.
```
$ gsd eval -w examples/evaluation.gsd
```

This should print the result of the execution plus its trace.
```
<Int> 3 : Int : Int

==BEGIN TRACE==
[[ (<Int> (<?O> Foo {x=<Int> (<Int> 2 : Int) : ?} : ?O).x : Int) + (<Int> 1 : Int) ]]
[[ <Int> (<?O> Foo {x=<Int> (<Int> 2 : Int) : ?} : ?O).x : Int ]] + (<Int> 1 : Int)
(<Int> [[ (<?O> Foo {x=<Int> (<Int> 2 : Int) : ?} : ?O).x ]] : Int) + (<Int> 1 : Int)
(<Int> [[ <?O> Foo {x=<Int> (<Int> 2 : Int) : ?} : ?O ]].x : Int) + (<Int> 1 : Int)
(<Int> (<?O> [[ Foo {x=<Int> (<Int> 2 : Int) : ?} ]] : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> Foo {x=[[ <Int> (<Int> 2 : Int) : ? ]]} : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> Foo {x=<Int> [[ <Int> 2 : Int ]] : ?} : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> Foo {x=<Int> (<Int> [[ 2 ]] : Int) : ?} : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> Foo {x=<Int> [[ <Int> 2 : Int ]] : ?} : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> Foo {x=[[ <Int> 2 : ? ]]} : ?O).x : Int) + (<Int> 1 : Int)
(<Int> (<?O> [[ Foo {x=<Int> 2 : ?} ]] : ?O).x : Int) + (<Int> 1 : Int)
(<Int> [[ <?O> Foo {x=<Int> 2 : ?} : ?O ]].x : Int) + (<Int> 1 : Int)
(<Int> [[ <Int> 2 : ? ]] : Int) + (<Int> 1 : Int)
[[ <Int> 2 : Int ]] + (<Int> 1 : Int)
(<Int> 2 : Int) + [[ <Int> 1 : Int ]]
(<Int> 2 : Int) + (<Int> [[ 1 ]] : Int)
(<Int> 2 : Int) + [[ <Int> 1 : Int ]]
<Int> 3 : Int
==END TRACE==
```

To differentiate evidences from types in the plain output, evidences are surrounded by angle brackets (`<T>`).
At each step, the sub-expression being evaluated is surrounded with double square brackets (`[[ e ]]`)`.

Only the first example is active, the second one is commented out.

To try the second example remove the comment at the start of line 8 and
run the interpreter again.
```
-- This should evaluate to 3
(Foo {x = 2} ).x + 1

-- This should fail with a transitivity error between Int and ?D
(Foo {x = 2}).x : ?D -- Uncomment this line
```
As expected, the interpreter throws a transitivity error, because
`Int` is not a datatype. In its current form the GSD interpreter
do not print traces for failed executions.
```
$ gsd eval -w examples/evaluation.gsd

Consistent transitivity between Int and ?D is not defined.
```

# The language

The syntax for expressions and declarations in GSD are as follows. Note that the syntax is indentation sensitive.

|     |    |                                                                                |                                               |
| --: | :-: | :----------------------------------------------------------------------------- | :-------------------------------------------- |
| *n* | := | ... &#124; \(-1\) &#124; 0 &#124; 1 &#124; ...                                                     | Integer literals                              |
| *s* | := | `"..."` &#124; `'...'`                                                                | String literals                               |
| *k* | := |  n &#124; s                                                                      | Literals                                      |
| *c* | := |  [A-Z][a-zA-Z0-9]*                                                     | Constructor name                      |
| *D* | := |  [A-Z][a-zA-Z0-9]*                                                     | Datatype name              |
| *x* | := |  [a-z][a-zA-Z0-9]*                                                     | Identifier name            |
| *l* | := |  [a-z][a-zA-Z0-9]*                                                     | Label name                 |
| *T* | := | **`?`**                                                                     | The unknown type                              |
|     |    | **`?D`**                                                                 | The unknown datatype  |
|     |    | **`?O`**                                                                 | The unknown open datatype |
|     |    | *D*                                                             | Datatype |
|     |    | **`Int`**                                                                   | Integer type                                  |
|     |    | **`String`**                                                               | Boolean type                                  |
|     |    | *T*<sub>1</sub> **`->`** *T*<sub>2</sub>                                       | Function type                                 |
|     |    | **`(`** *T* **`)`**                                                            |                                               |
| *e* | := | *x*                                                                            | Bound identifier                              |
|     |    | *k*                                                                            | Literal                                       |
|     |    | **`(`** *e* **`)`**                                                            | Parenthesis                                   |
|     |    | *e* **`:`** *T*                                                                | Type ascription                               |
|     |    | *c* *e*<sub>1</sub> ... *e*<sub>n</sub>                             | Positional constructor         |
|     |    | *c* **`{`** *l*<sub>1</sub> **`=`** *e*<sub>1</sub> **`,`** ... **`,`** *l*<sub>n</sub> **`=`** *e*<sub>n</sub> **`}`** | Positional constructor         |
|     |    | **`\`** *x* **`:`** *T* **`=>`** *e*                                        | Function                                      |
|     |    | **`\`** *x* **`=>`** *e*                                        | |
|     |    | *e* **`.`** *l*                                         | Field access                       |
|     |    | **`match`** *e*<sub>0</sub> **`with`** <br> &nbsp;&nbsp; *p*<sub>1</sub> **`=>`** *e*<sub>1</sub> <br> &nbsp;&nbsp; ... <br> &nbsp;&nbsp; *p*<sub>n</sub> **`=>`** *e*<sub>n</sub> | Conditional                                   |
|     |    | *e*<sub>1</sub> *e*<sub>2</sub>                                                | Application                                   |
|     |    | *e*<sub>1</sub> *op* *e*<sub>2</sub>                                     | Binary operations                     |
|     |    | **`if`** *e*<sub>1</sub> <br> &nbsp;&nbsp; **`then`** *e*<sub>2</sub> <br> &nbsp;&nbsp; **`else`** *e*<sub>3</sub> | Conditional                                   |
|     |    | **`let`** *x*<sub>1</sub> **`=`** *e*<sub>1</sub> <br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ... <br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *x*<sub>n</sub> **`=`** *e*<sub>n</sub> <br> **`in`** *e*<sub>0</sub> | Let binding                                   |
| *p* | := |  *c* *x*<sub>1</sub> ... *x*<sub>n</sub>               | Constructor pattern |
|  | |  `__`              | Default pattern |
| *op* | := |  **`+`** &#124;  **`-`** &#124; **`*`** &#124; **`/`** &#124; **`==`**                                            | Operators        |
| | | | |
| *def* | := |  [ **`open`** &#124; **`closed`** ]? **`data`** *D* **`=`** *C*<sub>1</sub> **<code> &#124; </code>** ... **<code> &#124; </code>** *C*<sub>n</sub> | Datatype definition |
| *C* | := | *c* **`{`** *l*<sub>1</sub> **`:`** *T*<sub>1</sub> **`,`** ... **`,`** *l*<sub>n</sub> **`:`** *T*<sub>n</sub> **`}`** | Constructor definition |


