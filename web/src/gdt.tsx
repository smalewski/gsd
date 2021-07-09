const gdtMode = () => {

    function switchState(source: Stream, setState: SetState, f: State): (string | null) {
        setState(f);
        return f(source, setState);
    }

    // These should all be Unicode extended, as per the Haskell 2010 report
    var smallRE = /[a-z_]/;
    var largeRE = /[A-Z]/;
    var digitRE = /\d/;
    var idRE = /[a-z_A-Z0-9]/;
    var symbolRE = /[-!#$%&*+.\/<=>?@\\^|~:]/;
    var specialRE = /[(),;[\]`{}]/;
    var whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer

    type Stream = {
        next: () => string;
        peek: () => string;
        eat: (match: string | RegExp | ((char: string) => boolean)) => string;
        eatWhile: (match: string | RegExp | ((char: string) => boolean)) => boolean;
        match: (pattern: RegExp) => string[];
        skipToEnd: () => void;
        eol: () => boolean;
        sol: () => boolean;
        eatSpace: () => boolean;
        current: () => string;
        backUp: (n: number) => void;
    }

    type SetState = (f: State) => void
    type State = (source: Stream, setState: SetState) => (null | string)
    type StateF = { f: State }

    function offset(source: Stream): number {
        return source.current().length;
    }

    function definition(source: Stream, setState: SetState): (null | string) {
        setState(normal);
        source.eatWhile(idRE);
        const endPosDef: number = offset(source);
        source.eatSpace();

        var ch = source.next();

        if (ch == null) {
            return "variable-2";
        }

        var col: number;

        // Type or constant definition
        if (ch == ':' || ch == '=') {
            col = offset(source);
            source.backUp(col - endPosDef);
            return "def";
        }

        // Function definition
        while (ch != null && smallRE.test(ch)) {
            source.eatWhile(idRE);
            source.eatSpace();
            ch = source.next();
        }
        if (ch == '=') {
            col = offset(source);
            source.backUp(col - endPosDef);
            return "def";
        }

        // Expression
        col = offset(source);
        source.backUp(col - endPosDef);
        return "variable-2";
    }

    function normal(source: Stream , setState: SetState): (null | string) {
        if (source.eatWhile(whiteCharRE)) {
            return null;
        }

        // Declarations
        if (source.sol()) {
            var cdef = source.peek();

            if (smallRE.test(cdef)) {
                return switchState(source, setState, definition);
            }
        }

        var ch = source.next();

        // Multi line comments
        if (specialRE.test(ch)) {
            if (ch == '{' && source.eat('-')) {
                return switchState(source, setState, ncomment("comment", 1));
            }
            return null;
        }

        // String literal
        if (ch == '"' || ch == '\'') {
            return switchState(source, setState, stringLiteral(ch));

        }

        // Data and constructors
        if (largeRE.test(ch)) {
            source.eatWhile(idRE);
            return "type";
        }

        // Unknown types
        if (ch == '?') {
            source.eat(/[DU]/);
            return "type";
        }

        if ((ch == '=' || ch == '-') && source.peek() == '>') {
            source.next();
            return "operator";
        }

        // Numbers
        if (digitRE.test(ch)) {
            source.eatWhile(digitRE);
            var t = "number";
            if (source.match(/^\.\d+/)) {
                t = "number";
            }
            return t;
        }

        // Identifiers and labels
        if (smallRE.test(ch)) {
            var t = "variable-2";
            source.eatWhile(idRE);
            return t;
        }

        // Single line commment
        if (symbolRE.test(ch)) {
            if (ch == '-' && source.eat(/-/)) {
                source.eatWhile(/-/);
                source.skipToEnd();
                return "comment";
            }
        }

        return "error";
    }

    function ncomment(type: string, nest: number): State {

        if (nest == 0) {
            return normal;
        }

        return (source: Stream, setState: SetState): string => {
            var currNest = nest;
            while (!source.eol()) {
                var ch = source.next();
                if (ch == '{' && source.eat('-')) {
                    ++currNest;
                }
                else if (ch == '-' && source.eat('}')) {
                    --currNest;
                    if (currNest == 0) {
                        setState(normal);
                        return type;
                    }
                }
            }
            setState(ncomment(type, currNest));
            return type;
        };
    }

    function stringLiteral(delim: string): State {
        return (source: Stream, setState: SetState): string => {
            while (!source.eol()) {
                var ch = source.next();
                if (ch == delim) {
                    setState(normal);
                    return "string";
                }
            }
            setState(normal);
            return "string error";
        }
    }

    const wellKnownWords = (() => {
        var wkw: Record<string, string> = {};

        function setType(t: string): (...args: string[]) => void {
            return function(...args: string[]): void {
                for (var i: number = 0; i < args.length; i++)
                    wkw[args[i]] = t;
            };
        }

        setType("keyword")(
            "data", "open", "closed",
            "match", "with", "if",
            "then", "else", "let", "in"
        );

        setType("operator")(
            "=>", "->", ":", "\\", "|", "=",
            "{", "}", "(", ")", "."
        );

        setType("operator")(
            "+", "-", "*", "/"
        );

        return wkw;
    })();

    return {
        startState: function(): StateF { return { f: normal }; },
        copyState: function(s: StateF): StateF { return { f: s.f }; },

        token: function(stream: Stream, state: StateF) {
            var t = state.f(stream, function(s: State) { state.f = s; });
            var w = stream.current();
            return wellKnownWords.hasOwnProperty(w) ? wellKnownWords[w] : t;
        },

        blockCommentStart: "{-",
        blockCommentEnd: "-}",
        lineComment: "--"
    };
};

export default gdtMode;
