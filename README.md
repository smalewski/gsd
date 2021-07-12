GSD (Gradually Structured Data).

# Installation

There are two ways to install the GSD interpreter:
natively in your machine, or virtually
(using a Vagrant VM or a Docker container).
GSD is written in Haskell, so if you already have
`stack` in your system, using GSD natively may be
the easiest alternative.

## Building GSD natively

To build the source code you need [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
, a package manager for Haskell.

With stack installed, you only need to clone the
repository and use `stack install`.
The compiled binary will be in `.local/bin/gsd`.

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
$ docker run -it gsd
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
  -? --help      Display help message
  -V --version   Print version information

gsd [eval] [OPTIONS] FILE
  Evaluate source file.

Matching strategies:
  -c --complete  Complete strategy
  -e --exact     Exact strategy
  -s --sound     Sound strategy
Output formats:
  -p --plain     Plain text
  -l --latex     Latex formated

gsd server [OPTIONS]

  -p --port=NUM  Default port: 8001

```

## Interpreter

To interpret a source file, use the command `eval` followed by
the file's name.
```
$ gsd eval examples/bas-1.gsd
```

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

### Running the web client


# The language

The syntax for expressions and declarations is as follows. Note that the syntax is indentation sensitive.

|     |    |                                                                                |                                               |
| --: | :-: | :----------------------------------------------------------------------------- | :-------------------------------------------- |
| *n* | := | ... | \(-1\) | 0 | 1 | ...                                                     | Integer literals                              |
| *s* | := | `"..."` | `'...'`                                                                | String literals                               |
| *k* | := |  n | s                                                                      | Literals                                      |
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


# OOPSLA 2021 Artifact Evaluation

What can be reproduced from the GSD paper?

- You should be able to run every example from the paper.
