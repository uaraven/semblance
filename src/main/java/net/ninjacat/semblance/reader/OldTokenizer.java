/*
 * Semblance
 *
 * Copyright 2013 Oleksiy Voronin <ovoronin@gmail.com>
 * http://ninjacat.net
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.ParsingException;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

/**
 * Created by: raven
 * Date: 02.12.12 14:07
 */
public class OldTokenizer {
    private static final Pattern INTEGER = Pattern.compile("(\\+|\\-){0,1}\\d+");
    private static final Pattern NUMBER = Pattern.compile("(\\+|\\-){0,1}\\d+(\\.\\d*){0,1}");
    private static final String DELIMITERS = "(); \t\n\r\"'#";
    private static final String DELIMITERS_IN_QUOTES = "\"\n\r";
    private static final String COMMENT = ";";

    private final StringTokenizer tokenizer;

    public OldTokenizer(String input) {
        this.tokenizer = new StringTokenizer(input, DELIMITERS, true);
    }

    public static java.util.List<Token> tokenize(String input) throws ParsingException {
        OldTokenizer tokenizer = new OldTokenizer(input);
        return tokenizer.tokenize();
    }

    java.util.List<Token> tokenize() throws ParsingException {
        java.util.List<Token> result = new ArrayList<Token>();
        int line = 1;
        int pos = 1;
        while (tokenizer.hasMoreElements()) {
            int tokenPos = pos;
            String token = tokenizer.nextToken(DELIMITERS);
            pos += token.length();
            Token.TokenType type = determineType(token);
            if (type == Token.TokenType.Comment) {
                while (type != Token.TokenType.CarriageReturn) {
                    pos += token.length();
                    token = tokenizer.nextToken();
                    type = determineType(token);
                }
            }
            if (type == Token.TokenType.StringQuote) {
                token = tokenizer.nextToken(DELIMITERS_IN_QUOTES);
                pos += token.length();
                String nextToken = tokenizer.nextToken(DELIMITERS);
                type = Token.TokenType.String;
                if (determineType(nextToken) != Token.TokenType.StringQuote) {
                    throw new ParsingException("Unterminated string", new SourceInfo(line, pos));
                }
                pos += nextToken.length();
            }
            if (type == Token.TokenType.CarriageReturn) {
                line += 1;
                pos = 1;
            }
            if (type != Token.TokenType.Whitespace && type != Token.TokenType.CarriageReturn) {
                result.add(new Token(token, type, line, tokenPos));
            }
        }

        return result;
    }

    private Token.TokenType determineType(CharSequence token) {
        if ("\"".equals(token)) {
            return Token.TokenType.StringQuote;
        }
        if ("\n".equals(token)) {
            return Token.TokenType.CarriageReturn;
        }
        if ("(".equals(token)) {
            return Token.TokenType.OpenParens;
        }
        if (")".equals(token)) {
            return Token.TokenType.CloseParens;
        }
        if ("'".equals(token)) {
            return Token.TokenType.Quote;
        }
        if ("#".equals(token)) {
            return Token.TokenType.Special;
        }
        if (isInteger(token)) {
            return Token.TokenType.Integer;
        }
        if (isNumber(token)) {
            return Token.TokenType.Double;
        }
        if (isWhitespace(token)) {
            return Token.TokenType.Whitespace;
        }
        if (COMMENT.equals(token)) {
            return Token.TokenType.Comment;
        }
        return Token.TokenType.Symbol;
    }

    private boolean isWhitespace(CharSequence token) {
        return " ".equals(token) || "\t".equals(token) || "\r".equals(token);
    }

    private boolean isInteger(CharSequence token) {
        return INTEGER.matcher(token).matches();
    }

    private boolean isNumber(CharSequence token) {
        return NUMBER.matcher(token).matches();
    }
}
