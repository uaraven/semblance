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

    public Token(final String value, final TokenType type, final SourceInfo sourceInfo) {
        this.value = value;
        this.type = type;
        position = sourceInfo;
    }

    public static Token eof(final SourceInfo position) {
        return new Token("", TokenType.Eof, position);
    }

    public static Token whitespace(final SourceInfo position) {
        return new Token("", TokenType.Whitespace, position);
    }

    public static Token string(final String sval, final SourceInfo sourceInfo) {
        return new Token(sval, TokenType.String, sourceInfo);
    }

    public static Token openParen(final SourceInfo sourceInfo) {
        return new Token("(", TokenType.OpenParens, sourceInfo);
    }

    public static Token closeParen(final SourceInfo sourceInfo) {
        return new Token(")", TokenType.CloseParens, sourceInfo);
    }

    public static Token openBracket(final SourceInfo sourceInfo) {
        return new Token("[", TokenType.OpenBracket, sourceInfo);
    }

    public static Token closeBracket(final SourceInfo sourceInfo) {
        return new Token("]", TokenType.CloseBracket, sourceInfo);
    }

    public static Token carriageReturn(final SourceInfo sourceInfo) {
        return new Token("\n", TokenType.CarriageReturn, sourceInfo);
    }

    public static Token special(final char ttype, final SourceInfo sourceInfo) {
        return new Token(Character.toString(ttype), TokenType.Special, sourceInfo);
    }

    public static Token symbol(final String sval, final SourceInfo sourceInfo) {
        return new Token(sval, Token.TokenType.Symbol, sourceInfo);
    }

    public static Token doubleToken(final double nval, final SourceInfo sourceInfo) {
        return new Token(String.valueOf(nval), Token.TokenType.Double, sourceInfo);
    }

    public static Token doubleToken(final String nval, final SourceInfo sourceInfo) {
        return new Token(nval, Token.TokenType.Double, sourceInfo);
    }

    public static Token integer(final String val, final SourceInfo sourceInfo) {
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

    @SuppressWarnings("all")
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
        int result = null != value ? value.hashCode() : 0;
        result = 31 * result + (null != type ? type.hashCode() : 0);
        return result;
    }

    /**
     * Enumeration of supported token types.
     */
    public enum TokenType {
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
