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

/**
 * Created by: raven
 * Date: 02.12.12 14:08
 */
public class Token {
    private final String value;
    private final TokenType type;
    private final SourceInfo position;

    public Token(String value, TokenType type, SourceInfo sourceInfo) {
        this.value = value;
        this.type = type;
        this.position = sourceInfo;
    }

    public static Token eof(SourceInfo position) {
        return new Token("", TokenType.Eof, position);
    }

    public static Token whitespace(SourceInfo position) {
        return new Token("", TokenType.Whitespace, position);
    }

    public static Token string(String sval, SourceInfo sourceInfo) {
        return new Token(sval, TokenType.String, sourceInfo);
    }

    public static Token openParen(SourceInfo sourceInfo) {
        return new Token("(", TokenType.OpenParens, sourceInfo);
    }

    public static Token closeParen(SourceInfo sourceInfo) {
        return new Token(")", TokenType.CloseParens, sourceInfo);
    }

    public static Token openBracket(SourceInfo sourceInfo) {
        return new Token("[", TokenType.OpenBracket, sourceInfo);
    }

    public static Token closeBracket(SourceInfo sourceInfo) {
        return new Token("]", TokenType.CloseBracket, sourceInfo);
    }

    public static Token carriageReturn(SourceInfo sourceInfo) {
        return new Token("\n", TokenType.CarriageReturn, sourceInfo);
    }

    public static Token special(char ttype, SourceInfo sourceInfo) {
        return new Token(Character.toString(ttype), TokenType.Special, sourceInfo);
    }

    public static Token symbol(String sval, SourceInfo sourceInfo) {
        return new Token(sval, Token.TokenType.Symbol, sourceInfo);
    }

    public static Token doubleToken(double nval, SourceInfo sourceInfo) {
        return new Token(String.valueOf(nval), Token.TokenType.Double, sourceInfo);
    }

    public static Token doubleToken(String nval, SourceInfo sourceInfo) {
        return new Token(nval, Token.TokenType.Double, sourceInfo);
    }

    public static Token integer(String val, SourceInfo sourceInfo) {
        return new Token(val, Token.TokenType.Integer, sourceInfo);
    }

    public String getValue() {
        return value;
    }

    public TokenType getType() {
        return type;
    }

    public SourceInfo getSourceInfo() {
        return position;
    }

    @Override
    public String toString() {
        return '"' + value + "\" (" + type.toString() + ')';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Token)) return false;

        Token token = (Token) o;

        if (type != token.type) return false;
        if (value != null ? !value.equals(token.value) : token.value != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = value != null ? value.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
    }

    public static enum TokenType {
        Integer,
        Double,
        String,
        Symbol,
        OpenParens,
        CloseParens,
        OpenBracket,
        CloseBracket,
        CarriageReturn,
        Whitespace,
        Comment,
        Special,
        Eof
    }
}
