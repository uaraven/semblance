package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.compile.UnterminatedStringException;

import java.io.*;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Lexer. Reads stream of characters and converts it into list of tokens
 * Created on 25/02/15.
 */
@SuppressWarnings("MagicCharacter")
public class ReaderStream {

    private static final Pattern INTEGER = Pattern.compile("(\\+|\\-)?\\d+");
    private static final Pattern DOUBLE = Pattern.compile("(\\+|\\-)?\\d+(\\.\\d*)?");

    private final Set<Character> specials;
    private final StreamTokenizer tokenizer;

    private int linePosition;

    private ReaderStream(final InputStream inputStream) {
        final InputStreamReader reader = new InputStreamReader(new BufferedInputStream(inputStream));
        specials = new HashSet<>();
        tokenizer = setupTokenizer(reader);
        linePosition = 0;
    }

    static ReaderStream readStream(final InputStream inputStream) {
        return new ReaderStream(inputStream);
    }

    static ReaderStream readString(final String text) {
        return new ReaderStream(new ByteArrayInputStream(text.getBytes(Charset.forName("utf-8"))));
    }

    void registerSpecial(final char specialCharacter) {
        specials.add(specialCharacter);
    }

    SourceInfo currentPosition() {
        return new SourceInfo(tokenizer.lineno(), linePosition);
    }

    List<Token> tokenize() throws ParsingException {
        final List<Token> tokens = new LinkedList<>();
        Token token;
        do {
            token = nextToken();
            if (!shouldIgnore(token)) {
                tokens.add(token);
            }
        } while (Token.TokenType.Eof != token.getType());
        return tokens;
    }

    private StreamTokenizer setupTokenizer(final InputStreamReader reader) {
        final StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
        resetTokenizer(streamTokenizer);
        return streamTokenizer;
    }

    private void resetTokenizer(final StreamTokenizer streamTokenizer) {
        streamTokenizer.resetSyntax();
        streamTokenizer.wordChars('a', 'z');
        streamTokenizer.wordChars('A', 'Z');
        streamTokenizer.wordChars('0', '9');
        streamTokenizer.wordChars(128 + 32, Integer.MAX_VALUE);
        streamTokenizer.wordChars('!', '!');
        streamTokenizer.wordChars('$', '&');
        streamTokenizer.wordChars('*', '-');
        streamTokenizer.wordChars('/', '/');
        streamTokenizer.wordChars(':', ':');
        streamTokenizer.whitespaceChars(0, ' ');
        streamTokenizer.commentChar(';');
        streamTokenizer.ordinaryChar('[');
        streamTokenizer.ordinaryChar(']');
        streamTokenizer.eolIsSignificant(true);
    }

    private void resetTokenizerForString() {
        tokenizer.resetSyntax();
        tokenizer.wordChars('a', 'z');
        tokenizer.wordChars('A', 'Z');
        tokenizer.wordChars('0', '9');
        tokenizer.wordChars(128 + 32, Integer.MAX_VALUE);
        tokenizer.ordinaryChar(' ');
        tokenizer.ordinaryChar('\t');
        tokenizer.ordinaryChar('\\');
        tokenizer.eolIsSignificant(true);
    }

    private void resetTokenizerForEscape() {
        tokenizer.resetSyntax();
        tokenizer.eolIsSignificant(true);
    }

    private Token nextToken() throws ParsingException {
        try {
            final int token = tokenizer.nextToken();
            final Token result;
            switch (token) {
                case StreamTokenizer.TT_WORD:
                    result = parseWord();
                    break;
                case StreamTokenizer.TT_EOF:
                    result = Token.eof(currentPosition());
                    break;
                case (int) '-':
                    result = parseNumber(true);
                    break;
                case (int) '"':
                    result = parseString();
                    break;
                default:
                    result = parseChar();
                    break;
            }
            linePosition += result.getValue().length();
            return result;
        } catch (final IOException e) {
            throw new ParsingException("Failed to read input", e, currentPosition());
        }
    }

    private Token parseString() throws IOException, UnterminatedStringException {
        resetTokenizerForString();
        final StringBuilder builder = new StringBuilder();
        try {
            while (true) {
                int token = tokenizer.nextToken();
                if ('"' == token) {
                    break;
                }
                if ('\\' == token) {
                    token = readEscapedChar();
                } else if (StreamTokenizer.TT_EOL == token || StreamTokenizer.TT_EOF == token) {
                    tokenizer.pushBack();
                    throw new UnterminatedStringException(currentPosition());
                }
                if (StreamTokenizer.TT_WORD == token) {
                    builder.append(tokenizer.sval);
                } else {
                    builder.append((char) token);
                }
            }
        } finally {
            resetTokenizer(tokenizer);
        }
        return Token.string(builder.toString(), currentPosition());

    }

    private int readEscapedChar() throws IOException {
        resetTokenizerForEscape();
        try {
            final int token = tokenizer.nextToken();
            switch (token) {
                case 't':
                    return '\t';
                case 'n':
                    return '\n';
                case 'r':
                    return '\r';
                case 'f':
                    return '\f';
                case 'b':
                    return '\b';
                default:
                    return token;
            }
        } finally {
            resetTokenizerForString();
        }
    }

    private Token parseChar() {
        switch (tokenizer.ttype) {
            case '(':
                return Token.openParen(currentPosition());
            case ')':
                return Token.closeParen(currentPosition());
            case '[':
                return Token.openBracket(currentPosition());
            case ']':
                return Token.closeBracket(currentPosition());
            case '\n':
                final Token token = Token.carriageReturn(currentPosition());
                linePosition = 0;
                return token;
            default:
                if (specials.contains((char) tokenizer.ttype)) {
                    return Token.special((char) tokenizer.ttype, currentPosition());
                } else {
                    return Token.symbol(String.valueOf((char) tokenizer.ttype), currentPosition());
                }
        }
    }

    private Token parseWord() throws IOException {
        if (isInteger(tokenizer.sval)) {
            return parseNumber(false);
        } else if (isDouble(tokenizer.sval)) {
            return Token.doubleToken(tokenizer.nval, currentPosition());
        } else {
            return Token.symbol(tokenizer.sval, currentPosition());
        }
    }

    private Token parseNumber(final boolean negative) throws IOException {
        final String val;
        if (negative) {
            final int token = tokenizer.nextToken();
            if (token == StreamTokenizer.TT_WORD && isInteger(tokenizer.sval)) {
                val = "-" + tokenizer.sval;
            } else {
                tokenizer.pushBack();
                val = tokenizer.sval;
            }
        } else {
            val = tokenizer.sval;
        }
        final int token = tokenizer.nextToken();
        if (token == '.' || token == 'e' || token == 'E') {
            return parseDouble(val + Character.toString((char) token));
        } else {
            tokenizer.pushBack();
            return Token.integer(val, currentPosition());
        }
    }

    private Token parseDouble(String s) throws IOException {
        final int token = tokenizer.nextToken();
        if (token != StreamTokenizer.TT_WORD) {
            tokenizer.pushBack();
        } else {
            s += tokenizer.sval;
        }
        return Token.doubleToken(s, currentPosition());
    }

    private boolean isInteger(final String nval) {
        return INTEGER.matcher(nval).matches();
    }

    private boolean isDouble(final String nval) {
        return DOUBLE.matcher(nval).matches();
    }

    private boolean shouldIgnore(final Token token) {
        return token.getType() == Token.TokenType.CarriageReturn ||
                token.getType() == Token.TokenType.Comment ||
                token.getType() == Token.TokenType.Whitespace ||
                token.getType() == Token.TokenType.Eof;
    }
}
