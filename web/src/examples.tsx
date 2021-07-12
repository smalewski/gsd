type Example = {
    title: string,
    description: string,
    body: string
};

const examples: Example[] = [
    {
        title: "BAS Version 1",
        description: "Section 2. Figure 1.",
        body: `serve jsonReq = toJSON (handleRequest (fromJSON jsonReq))

handleRequest request =
  withValidKey request (\\r => match r with
                                Plus key x y  => Success {r = (x + y)}
                                Minus key x y => Success {r = (x - y)}
                                _ => Fail {r = unknownMethodError})

withValidKey r action =
  if isValidKey r.key
    then action r
    else Fail {r=invalidKeyError}

invalidKeyError = "Error: invalid key"
unknownMethodError = "Error: unknown method"
isValidKey x = True

serve '{"Plus": {"key": 10, "x": 1, "y": 2}}'
-- serve '{"Times": {"key": 10, "x": 1, "y": 2}}'
`
    },
    {
        title: "BAS Version 2",
        description: "Section 2. Figure 2.",
        body: `data Response = Success {x : ?} | Fail {msg : String}
open data Error
open data Request

serve : String -> String
serve jsonReq = toJSON (handleRequest (fromJSON jsonReq))

handleRequest : Request -> Response
handleRequest request =
  withValidKey request (\\r : Request => match r with
                                          Plus key x y  => Success (x + y)
                                          Minus key x y => Success (x - y)
                                          _             => Fail (throw MethodError))


withValidKey : Request -> (Request -> Response) -> Response
withValidKey r action =
  if isValidKey r.key
    then action r
    else Fail (throw InvalidKeyError)


throw : Error -> String
throw err =
  match err with
    MethodError           => "unknown method"
    InvalidKeyError       => "invalid key"
    IllFormedRequestError => "request missing 'key'"
    _                     => "unknown error"

isValidKey : ? -> Bool
isValidKey key = True

serve '{"Plus": {"key": 10, "x": 1, "y": 2}}'
-- serve '{"Times": {"key": 10, "x": 1, "y": 2}}'
`
    },
    {
        title: "BAS Version 3",
        description: "Section 2. Figure 3.",
        body: `data Response = Success { x : ? } | Fail { msg : String }

-- Declaring possible constructors
open data Error = MethodError
                | InvalidKeyError
                | IllFormedRequestError

open data Request = Plus  { key : Int, x : ?, y : ? }
                  | Minus { key : Int, x : ?, y : ? }

serve : String -> String
serve jsonReq = toJSON (handleRequest (fromJSON jsonReq))

handleRequest : Request -> Response
handleRequest request =
  withValidKey request (\\r : Request =>
                          match r with
                            Plus key x y  => Success (x + y)
                            Minus key x y => Success (x - y)
                            Not key x     => Success (not x)
                            _             => Fail (throw MethodError))


withValidKey : Request -> (Request -> Response) -> Response
withValidKey r action =
  if isValidKey r.key
    then action r
    else Fail (throw InvalidKeyError)


throw : Error -> String
throw err =
  match err with
    MethodError           => "unknown method"
    InvalidKeyError       => "invalid key"
    IllFormedRequestError => "request missing 'key'"
    _                     => "unknown error"

isValidKey : Int -> Bool
isValidKey key = True

serve '{"Plus": {"key": 10, "x": 1, "y": 2}}'
-- serve '{"Times": {"key": 10, "x": 1, "y": 2}}'`
    },
    {
        title: "BAS Version 4",
        description: "Section 2. Figure 4.",
        body: `data Response = Success {x : Data} | Fail {msg : String}
data Data = N {x : Int} | B {p : Bool}
data Error = InvalidKeyError
           -- | IllFormedRequestError is no longer possible
           -- | MethodError {name : String} is no longer possible
data Request = Plus {key : Int, x : Int, y : Int}
             | Minus {key : Int, x : Int, y : Int}
             | Not {key : Int, p : Bool}

serve : String -> String
serve jsonReq = toJSON (handleRequest (fromJSON jsonReq))

handleRequest : Request -> Response
handleRequest request =
  withValidKey request (\\r : Request =>
                          match r with
                            Plus key x y  => Success (N (x + y))
                            Minus key x y => Success (N (x - y))
                            Not key x     => Success (B (not x)))
                            -- _             => Fail (throw MethodError) -- ruled out!

withValidKey : Request -> (Request -> Response) -> Response
withValidKey r action =
  if isValidKey r.key
    then action r
    else Fail (throw InvalidKeyError)

isValidKey : Int -> Bool
isValidKey n = True

throw : Error -> String
throw err = match err with
              InvalidKeyError => "invalid key"

serve '{"Plus": {"key": 10, "x": 1, "y": 2}}'
-- serve '{"Times": {"key": 10, "x": 1, "y": 2}}'`
    },
    {
        title: "Flatten function",
        description: "",
        body: `data List = Cons {hd : ?, tl : List} | Nil

flatten : ?D -> List
flatten l =
  match l with
    Nil       => Nil
    Cons x xs => append (flatten x) (flatten xs)
    _         => Cons l Nil

append : List -> List -> List
append l1 l2 =
  match l1 with
    Nil       => l2
    Cons x xs => Cons x (append xs l2)

flatten (Cons (Cons (Foo {x = 3}) Nil) (Cons (Cons (Bar {x = 4}) Nil) Nil))`
    },
    {
        title: "Lambda calculus interpreter",
        description: "",
        body: `data Assoc = Empty | Cons {k : ?, v : ?, tl : Assoc}

insert : Assoc -> ? -> ? -> Assoc
insert env x v = Cons x v env

lookup : ? -> Assoc -> ?
lookup x env =
  match env with
    Empty         => NotFound {var = x}
    Cons k v rest => if x == k then v else lookup x rest

-- Remove any constructor definition below.
-- Everything keeps working as before, but with less precise types.

open data Expr = Var {x : ?}
               | Lambda {x : ?, e : Expr}
               | App {e1 : Expr, e2 : Expr}

-- Try extending this interpreter with Pairs, or other feature,
-- without changing Expr's definition.

eval : Assoc -> Expr -> ?
eval env expr =
  match expr with
    Var x      => lookup x env
    Lambda x e => Clos {x = x, expr = e, env = env}
    App e1 e2  => let v1 = eval env e1
                      v2 = eval env e2
                   in (match v1 with
                        Clos x ex envx => eval (insert envx x v2) ex
                        _              => AppliedNonLambda {expr = v1})

-- \\x => x
id = Lambda { x = "x", e = Var { x = "x" } }
-- \\x => x x
omega = Lambda { x = "x", e = App { e1 = Var { x = "x" }, e2 = Var { x = "x" } } }
-- \\x => x x x
omega3 = Lambda {x = "x", e = App { e1 = App { e1 = Var { x = "x" }, e2 = Var { x = "x" }}, e2 = Var  { x = "x" }} }

-- (\\x => x x x) (\\x => x)
eval Empty (App { e1 = omega3, e2 = id })`
    },
    {
        title: "Meanings for validity of matches of static programs",
        description: "",
        body: `{-
 - This example explores the different meanings for the validity
 - of matches in a static program.
 -}

data A = A1 | A2
data B = B1

match A1 with
  A1 => 1     -- The match expression starts being only Sound with respect to A.

  -- A2 => 2  -- If you uncomment this case the match expression will be Exact
              -- with respect to A, and therefore Sound and Complete.

  -- B1 => 3  -- If you uncomment this case and the one above, the match expression
              -- will be Complete with respect to both A and B.`
    },
    {
        title: "Meanings for validity of matches of gradual programs",
        description: "",
        body: `{-
 - This example explores the different meanings for the validity
 - of matches in a gradual program.
 -}

data A = A1 | A2
data B = B1
data C = C1

match C1 : ?D with
  A1 => 1  -- The match expression starts being only Sound with respect to A.

  -- A2 => 2  -- After uncommenting this case the match expression will be Exact
              -- with respect to A, and therefore Sound and Complete.
              -- But unlike in the static example runtime match errors are still possible.

  -- B1 => 3  -- After uncommenting this case and the one above, the match expression
              -- will be Complete with respect to both A and B.
              -- But, as in the case before, runtime match errors are still possible.

-- Runtime match errors cannot be ruled out if the expression being matched has a gradual type.`
    },
    {
        title: "Unclassified data cannot inhabit a closed datatype",
        description: "",
        body: `data Closed -- Datatypes are closed by default
open data Open

Foo : Open    -- This typechecks
-- Foo : Closed  -- This does not`
    },
    {
        title: "Tutorial: Field access",
        description: "",
        body: `-- Field access for closed datatypes works as expected,
-- accesses to named parameters not present in the type are
-- ruled out by the typecheker.

data Closed = Foo {x : ?, y : Int} | Bar {x : ?, z : ?}

-- (Foo 1 2).a -- This does not type check.
-- (Foo 1 2).z -- But, they can still fail at runtime

-- For open datatypes, things work differently, field access will
-- always typecheck. The same goes for unclassified data.

open data Open = Baz {x : ?}

(Baz 1).y
-- Fizz.bar -- This also typechecks but fails at runtime.`
    },

];
export default examples;
