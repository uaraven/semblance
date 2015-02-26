package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.ParsingException;
import net.ninjacat.semblance.errors.UnterminatedStringException;

import java.io.*;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Created on 25/02/15.
 */
public class ReaderStream {

    private static final Pattern INTEGER = Pattern.compile("(\\+|\\-){0,1}\\d+");
    private static final Pattern NUMBER = Pattern.compile("(\\+|\\-){0,1}\\d+(\\.\\d*){0,1}");

    private final Set<Character> specials;
    private final StreamTokenizer tokenizer;

    private int linePosition;

    private ReaderStream(InputStream inputStream) {
        InputStreamReader reader = new InputStreamReader(new BufferedInputStream(inputStream));
        specials = new HashSet<>();
        tokenizer = setupTokenizer(reader);
        linePosition = 0;
    }

    public static ReaderStream readStream(InputStream inputStream) {
        return new ReaderStream(inputStream);
    }

    public static ReaderStream readString(String string) {
        return new ReaderStream(new ByteArrayInputStream(string.getBytes(Charset.forName("utf-8"))));
    }

    private StreamTokenizer setupTokenizer(InputStreamReader reader) {
        StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
        resetTokenizer(streamTokenizer);
        return streamTokenizer;
    }

    private void resetTokenizer(StreamTokenizer streamTokenizer) {
        streamTokenizer.resetSyntax();
        streamTokenizer.wordChars('a', 'z');
        streamTokenizer.wordChars('A', 'Z');
        streamTokenizer.wordChars('0', '9');
        streamTokenizer.wordChars(128 + 32, Integer.MAX_VALUE);
        streamTokenizer.whitespaceChars(0, ' ');
        streamTokenizer.commentChar('/');
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

    public void registerSpecial(char specialCharacter) {
        specials.add(specialCharacter);
    }

    public SourceInfo currentPosition() {
        return new SourceInfo(tokenizer.lineno(), linePosition);
    }

    private Token nextToken() throws ParsingException {
        try {
            int token = tokenizer.nextToken();
            Token result;
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
        } catch (IOException e) {
            throw new ParsingException("Failed to read input", e, currentPosition());
        }
    }

    private Token parseString() throws IOException, UnterminatedStringException {
        resetTokenizerForString();
        StringBuilder builder = new StringBuilder();
        try {
            while (true) {
                int token = tokenizer.nextToken();
                if (token == '"') {
                    break;
                } else if (token == '\\') {
                    token = readEscapedChar();
                } else if (token == StreamTokenizer.TT_EOL || token == StreamTokenizer.TT_EOF) {
                    tokenizer.pushBack();
                    throw new UnterminatedStringException(currentPosition());
                }
                if (token == StreamTokenizer.TT_WORD) {
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
            int token = tokenizer.nextToken();
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
            case '\n':
                Token token = Token.carriageReturn(currentPosition());
                linePosition = 0;
                return token;
            default:
                if (specials.contains((char) tokenizer.ttype)) {
                    return Token.special((char) tokenizer.ttype, currentPosition());
                }
        }
        return Token.whitespace(currentPosition());
    }

    private Token parseWord() throws IOException {
        if (isInteger(tokenizer.sval)) {
            return parseNumber(false);
        } else if (isDouble(tokenizer.sval)) {
            return new Token(String.valueOf(tokenizer.nval), Token.TokenType.Double, currentPosition());
        } else {
            return new Token(tokenizer.sval, Token.TokenType.Symbol, currentPosition());
        }
    }

    private Token parseNumber(boolean negative) throws IOException {
        String val;
        if (negative) {
            int token = tokenizer.nextToken();
            if (token == StreamTokenizer.TT_WORD && isInteger(tokenizer.sval)) {
                val = "-" + tokenizer.sval;
            } else {
                tokenizer.pushBack();
                val = tokenizer.sval;
            }
        } else {
            val = tokenizer.sval;
        }
        int token = tokenizer.nextToken();
        if (token == '.' || token == 'e' || token == 'E') {
            return parseDouble(val + Character.toString((char) token));
        } else {
            tokenizer.pushBack();
            return new Token(val, Token.TokenType.Integer, currentPosition());
        }
    }

    private Token parseDouble(String s) throws IOException {
        int token = tokenizer.nextToken();
        if (token != StreamTokenizer.TT_WORD) {
            tokenizer.pushBack();
        } else {
            s += tokenizer.sval;
        }
        return new Token(s, Token.TokenType.Double, currentPosition());
    }

    private boolean isInteger(String nval) {
        return INTEGER.matcher(nval).matches();
    }

    private boolean isDouble(String nval) {
        return NUMBER.matcher(nval).matches();
    }

    public List<Token> tokenize() throws ParsingException {
        List<Token> tokens = new LinkedList<>();
        Token token;
        do {
            token = nextToken();
            if (!shouldIgnore(token)) {
                tokens.add(token);
            }
        } while (token.getType() != Token.TokenType.Eof);
        return tokens;
    }

    private boolean shouldIgnore(Token token) {
        return token.getType() == Token.TokenType.CarriageReturn ||
                token.getType() == Token.TokenType.Comment ||
                token.getType() == Token.TokenType.Whitespace ||
                token.getType() == Token.TokenType.Eof;
    }
}
