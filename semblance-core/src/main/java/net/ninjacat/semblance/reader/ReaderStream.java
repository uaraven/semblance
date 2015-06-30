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
    private static final Pattern DOUBLE = Pattern.compile("((\\+|\\-)?\\d+(\\.\\d*)?)");
    private static final Pattern DOUBLE_SCI = Pattern.compile("(\\+|\\-)?\\d+(\\.\\d+)?(e|E)(\\+|\\-)?\\d+");

    private final Set<Character> specials;
    private final Tokenizer tokenizer;
    private final InputStreamReader reader;

    private int linePosition;

    private ReaderStream(final InputStream inputStream) {
        reader = new InputStreamReader(new BufferedInputStream(inputStream));
        specials = new HashSet<>();
        tokenizer = setupTokenizer();
        linePosition = 0;
    }

    /**
     * Closes reader stream and releases all underlying resources.
     */
    public void close() {
        try {
            reader.close();
        } catch (final IOException ignored) {
        }
    }

    static ReaderStream readStream(final InputStream inputStream) {
        return new ReaderStream(inputStream);
    }

    static ReaderStream readString(final String text) {
        return new ReaderStream(new ByteArrayInputStream(text.getBytes(Charset.forName("utf-8"))));
    }

    private static boolean isInteger(final String nval) {
        return INTEGER.matcher(nval).matches();
    }

    private static boolean isDouble(final String nval) {
        return DOUBLE.matcher(nval).matches();
    }

    private static boolean isScientificDouble(final String sval) {
        return DOUBLE_SCI.matcher(sval).matches();
    }

    void registerSpecial(final char specialCharacter) {
        specials.add(specialCharacter);
        tokenizer.registerSpecials(specials);
    }

    SourceInfo currentPosition() {
        return new SourceInfo(tokenizer.getLine(), linePosition);
    }

    List<Token> tokenize() throws ParsingException {
        setupTokenizer();
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

    private Tokenizer setupTokenizer() {
        return new Tokenizer(reader);
    }

    private void resetTokenizer(final Tokenizer streamTokenizer) {
        streamTokenizer.setupForGeneralParsing();
        streamTokenizer.registerSpecials(specials);
    }

    private void resetTokenizerForString() {
        tokenizer.setupForStringParsing();
    }

    private Token nextToken() throws ParsingException {
        try {
            final Tokenizer.TextType token = tokenizer.nextToken();
            final Token result;
            switch (token) {
                case WORD:
                    result = parseWord();
                    break;
                case EOF:
                    result = Token.eof(currentPosition());
                    break;
                default:
                    if (tokenizer.getTokenChar() == '\"') {
                        result = parseString();
                    } else {
                        result = parseChar();
                    }
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
                final Tokenizer.TextType tokenType = tokenizer.nextToken();
                final int token = tokenizer.getTokenChar();
                if ('\n' == token || '\r' == token || -1 == token) {
                    throw new UnterminatedStringException(currentPosition());
                }
                if (tokenType == Tokenizer.TextType.SYMBOL) {
                    if ('"' == token) {
                        break;
                    } else if ('\\' == token) {
                        builder.append((char) readEscapedChar());
                    } else {
                        builder.append((char) token);
                    }
                } else if (tokenType == Tokenizer.TextType.WORD) {
                    builder.append(tokenizer.getTokenValue());
                }
            }
        } finally {
            resetTokenizer(tokenizer);
        }
        return Token.string(builder.toString(), currentPosition());

    }

    private int readEscapedChar() throws IOException, UnterminatedStringException {
        try {
            final int token = tokenizer.nextChar();
            switch (token) {
                case -1:
                    throw new UnterminatedStringException(currentPosition());
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
        final char ttype = (char) tokenizer.getTokenChar();
        switch (ttype) {
            case '(':
                return Token.openParen(currentPosition());
            case ')':
                return Token.closeParen(currentPosition());
            case '[':
                return Token.openBracket(currentPosition());
            case ']':
                return Token.closeBracket(currentPosition());
            case '{':
                return Token.openBrace(currentPosition());
            case '}':
                return Token.closeBrace(currentPosition());
            case '\n':
                final Token token = Token.carriageReturn(currentPosition());
                linePosition = 0;
                return token;
            default:
                if (specials.contains(ttype)) {
                    return Token.special(ttype, currentPosition());
                } else {
                    return Token.symbol(String.valueOf(ttype), currentPosition());
                }
        }
    }

    private Token parseWord() throws IOException {
        final String sval = tokenizer.getTokenValue();
        if (isInteger(sval)) {
            return Token.integer(sval, currentPosition());// parseNumber(false);
        } else if (isDouble(sval)) {
            return Token.doubleToken(sval, currentPosition());
        } else if (isScientificDouble(sval)) {
            return Token.doubleToken(Double.parseDouble(sval), currentPosition());
        } else {
            return Token.symbol(sval, currentPosition());
        }
    }

    private boolean shouldIgnore(final Token token) {
        return Token.TokenType.CarriageReturn == token.getType() ||
                Token.TokenType.Comment == token.getType() ||
                Token.TokenType.Whitespace == token.getType() ||
                Token.TokenType.Eof == token.getType();
    }
}
