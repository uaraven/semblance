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
 * Parsed token representation
 */
@SuppressWarnings({"QuestionableName", "StaticMethodNamingConvention"})
public class Token {
    private final String value;
    private final TokenType type;
    private final SourceInfo position;

    /**
     * Creates a new token
     *
     * @param value      Token value.
     * @param type       Token type.
     * @param sourceInfo Source position information for the token.
     */
    public Token(final String value, final TokenType type, final SourceInfo sourceInfo) {
        this.value = value;
        this.type = type;
        position = sourceInfo;
    }

    /**
     * Creates end of file token.
     *
     * @param position Source code position.
     * @return New Token.
     */
    public static Token eof(final SourceInfo position) {
        return new Token("", TokenType.Eof, position);
    }

    /**
     * Creates string token.
     *
     * @param sval       String value.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token string(final String sval, final SourceInfo sourceInfo) {
        return new Token(sval, TokenType.String, sourceInfo);
    }

    /**
     * Creates open paren token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token openParen(final SourceInfo sourceInfo) {
        return new Token("(", TokenType.OpenParens, sourceInfo);
    }

    /**
     * Creates close paren token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token closeParen(final SourceInfo sourceInfo) {
        return new Token(")", TokenType.CloseParens, sourceInfo);
    }

    /**
     * Creates open bracket token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token openBracket(final SourceInfo sourceInfo) {
        return new Token("[", TokenType.OpenBracket, sourceInfo);
    }

    /**
     * Creates close bracket token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token closeBracket(final SourceInfo sourceInfo) {
        return new Token("]", TokenType.CloseBracket, sourceInfo);
    }

    /**
     * Creates open brace token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token openBrace(final SourceInfo sourceInfo) {
        return new Token("{", TokenType.OpenBrace, sourceInfo);
    }

    /**
     * Creates close brace token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token closeBrace(final SourceInfo sourceInfo) {
        return new Token("}", TokenType.CloseBrace, sourceInfo);
    }

    /**
     * Creates carriage return token.
     *
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token carriageReturn(final SourceInfo sourceInfo) {
        return new Token("\n", TokenType.CarriageReturn, sourceInfo);
    }

    /**
     * Creates special character token.
     *
     * @param ttype      Character.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token special(final char ttype, final SourceInfo sourceInfo) {
        return new Token(Character.toString(ttype), TokenType.Special, sourceInfo);
    }

    /**
     * Creates symbol token.
     *
     * @param sval       Symbol value.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token symbol(final String sval, final SourceInfo sourceInfo) {
        return new Token(sval, TokenType.Symbol, sourceInfo);
    }

    /**
     * Creates double token.
     *
     * @param nval       Double value.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token doubleToken(final double nval, final SourceInfo sourceInfo) {
        return new Token(String.valueOf(nval), TokenType.Double, sourceInfo);
    }

    /**
     * Creates double token.
     *
     * @param nval       String representation of double value.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token doubleToken(final String nval, final SourceInfo sourceInfo) {
        return new Token(nval, TokenType.Double, sourceInfo);
    }

    /**
     * Creates integer token.
     *
     * @param val        Integer value.
     * @param sourceInfo Source code position.
     * @return New Token.
     */
    public static Token integer(final String val, final SourceInfo sourceInfo) {
        return new Token(val, TokenType.Integer, sourceInfo);
    }

    /**
     * @return Token value.
     */
    public String getValue() {
        return value;
    }

    /**
     * @return Token type.
     */
    public TokenType getType() {
        return type;
    }

    /**
     * @return Token source code information.
     */
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
        OpenBrace,
        CloseBrace,
        CarriageReturn,
        Whitespace,
        Comment,
        Special,
        Eof
    }
}
