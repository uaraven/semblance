// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

/**
 * Author: Oleksii Voronin
 * Branched from CodeMirror's Clojure mode (by Hans Engel, based on implementation by Koh Zi Chun)
 */

(function (mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
        mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
        define(["../../lib/codemirror"], mod);
    else // Plain browser env
        mod(CodeMirror);
})(function (CodeMirror) {
    "use strict";

    CodeMirror.defineMode("semblance", function (options) {
        var BUILTIN = "builtin", COMMENT = "comment", STRING = "string", CHARACTER = "string-2",
            ATOM = "atom", NUMBER = "number", BRACKET = "bracket", KEYWORD = "keyword", VAR = "variable";
        var INDENT_WORD_SKIP = options.indentUnit || 2;
        var NORMAL_INDENT_UNIT = options.indentUnit || 2;

        function makeKeywords(str) {
            var obj = {}, words = str.split(" ");
            for (var i = 0; i < words.length; ++i) obj[words[i]] = true;
            return obj;
        }

        var atoms = makeKeywords("T F nil");

        var keywords = makeKeywords(
            "include var update list , ' quote ` backquote namespace use progn block fn defmacro funcall " +
            "return break loop recur defun defmacro");

        var builtins = makeKeywords("writeout writeout-cr + - / % * if cond select = != > < >= <= and or not list/find" +
            "map/keys map/values contains zip when until let return-from fn* car cdr do-list do-times print println");

        var indentKeys = makeKeywords(
            // Built-ins
            "namespace progn block fn fn* if cond select defmacro defun let do-list");

        var tests = {
            digit: /\d/,
            digit_or_colon: /[\d:]/,
            hex: /[0-9a-f]/i,
            sign: /[+-]/,
            exponent: /e/i,
            keyword_char: /[^\s\(\[\;\)\]]/,
            symbol: /[\w*+!\-\._?:<>\/\xa1-\uffff]/,
            block_indent: /^(?:def|with)[^\/]+$|\/(?:def|with)/
        };

        function stateStack(indent, type, prev) { // represents a state stack object
            this.indent = indent;
            this.type = type;
            this.prev = prev;
        }

        function pushStack(state, indent, type) {
            state.indentStack = new stateStack(indent, type, state.indentStack);
        }

        function popStack(state) {
            state.indentStack = state.indentStack.prev;
        }

        function isNumber(ch, stream) {
            // hex
            if (ch === '0' && stream.eat(/x/i)) {
                stream.eatWhile(tests.hex);
                return true;
            }

            // leading sign
            if (( ch == '+' || ch == '-' ) && ( tests.digit.test(stream.peek()) )) {
                stream.eat(tests.sign);
                ch = stream.next();
            }

            if (tests.digit.test(ch)) {
                stream.eat(ch);
                stream.eatWhile(tests.digit);

                if ('.' == stream.peek()) {
                    stream.eat('.');
                    stream.eatWhile(tests.digit);
                } else if ('/' == stream.peek()) {
                    stream.eat('/');
                    stream.eatWhile(tests.digit);
                }

                if (stream.eat(tests.exponent)) {
                    stream.eat(tests.sign);
                    stream.eatWhile(tests.digit);
                }

                return true;
            }

            return false;
        }

        // Eat character that starts after backslash \
        function eatCharacter(stream) {
            var first = stream.next();
            // Read special literals: backspace, newline, space, return.
            // Just read all lowercase letters.
            if (first && first.match(/[a-z]/) && stream.match(/[a-z]+/, true)) {
                return;
            }
            // Read unicode character: \u1000 \uA0a1
            if (first === "u") {
                stream.match(/[0-9a-z]{4}/i, true);
            }
        }

        return {
            startState: function () {
                return {
                    indentStack: null,
                    indentation: 0,
                    mode: false
                };
            },

            token: function (stream, state) {
                if (state.indentStack == null && stream.sol()) {
                    // update indentation, but only if indentStack is empty
                    state.indentation = stream.indentation();
                }

                // skip spaces
                if (state.mode != "string" && stream.eatSpace()) {
                    return null;
                }
                var returnType = null;

                switch (state.mode) {
                    case "string": // multi-line string parsing mode
                        var next, escaped = false;
                        while ((next = stream.next()) != null) {
                            if (next == "\"" && !escaped) {

                                state.mode = false;
                                break;
                            }
                            escaped = !escaped && next == "\\";
                        }
                        returnType = STRING; // continue on in string mode
                        break;
                    default: // default parsing mode
                        var ch = stream.next();

                        if (ch == "\"") {
                            state.mode = "string";
                            returnType = STRING;
                        } else if (ch == "\\") {
                            eatCharacter(stream);
                            returnType = CHARACTER;
                        } else if (ch == "'" && !( tests.digit_or_colon.test(stream.peek()) )) {
                            returnType = ATOM;
                        } else if (ch == ";") { // comment
                            stream.skipToEnd(); // rest of the line is a comment
                            returnType = COMMENT;
                        } else if (isNumber(ch, stream)) {
                            returnType = NUMBER;
                        } else if (ch == "(" || ch == "[" || ch == "{") {
                            var keyWord = '', indentTemp = stream.column(), letter;
                            /**
                             Either
                             (indent-word ..
                             (non-indent-word ..
                             (;something else, bracket, etc.
                             */

                            if (ch == "(") while ((letter = stream.eat(tests.keyword_char)) != null) {
                                keyWord += letter;
                            }

                            if (keyWord.length > 0 && (indentKeys.propertyIsEnumerable(keyWord) ||
                                tests.block_indent.test(keyWord))) { // indent-word
                                pushStack(state, indentTemp + INDENT_WORD_SKIP, ch);
                            } else { // non-indent word
                                // we continue eating the spaces
                                stream.eatSpace();
                                if (stream.eol() || stream.peek() == ";") {
                                    // nothing significant after
                                    // we restart indentation the user defined spaces after
                                    pushStack(state, indentTemp + NORMAL_INDENT_UNIT, ch);
                                } else {
                                    pushStack(state, indentTemp + stream.current().length, ch); // else we match
                                }
                            }
                            stream.backUp(stream.current().length - 1); // undo all the eating

                            returnType = BRACKET;
                        } else if (ch == ")" || ch == "]" || ch == "}") {
                            returnType = BRACKET;
                            if (state.indentStack != null && state.indentStack.type == (ch == ")" ? "(" : (ch == "]" ? "[" : "{"))) {
                                popStack(state);
                            }
                        } else if (ch == ":") {
                            stream.eatWhile(tests.symbol);
                            return ATOM;
                        } else {
                            stream.eatWhile(tests.symbol);

                            if (keywords && keywords.propertyIsEnumerable(stream.current())) {
                                returnType = KEYWORD;
                            } else if (builtins && builtins.propertyIsEnumerable(stream.current())) {
                                returnType = BUILTIN;
                            } else if (atoms && atoms.propertyIsEnumerable(stream.current())) {
                                returnType = ATOM;
                            } else {
                                returnType = VAR;
                            }
                        }
                }

                return returnType;
            },

            indent: function (state) {
                if (state.indentStack == null) return state.indentation;
                return state.indentStack.indent;
            },

            closeBrackets: {pairs: "()[]{}\"\"''"},
            lineComment: ";;"
        };
    });

    CodeMirror.defineMIME("text/x-semblance", "semblance");


});