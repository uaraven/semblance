package net.ninjacat.semblance.reader;

import net.ninjacat.smooth.collections.Collect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * StreamReader tokenizer with the ability to look forward and push back.
 * <p/>
 * This class is not thread-safe.
 */
public class Tokenizer {
    public static final int EOF_MARK = -1;
    private static final Set<Character> WHITESPACE = Collect.setOf(
            ' ', '\t', '\n', '\r'
    );
    private static final Set<Character> SYMBOLS = new HashSet<>();
    private static final int LOOKAHEAD_BUF_SIZE = 5;
    private final char[] cbuf = new char[LOOKAHEAD_BUF_SIZE];

    private final Set<Character> whitespace;
    private final Set<Character> symbols;
    private final List<Character> buffer;
    private final java.io.Reader reader;
    private final StringBuilder tokenValue;
    private int line;
    private int position;

    /**
     * Creates a new instance of Tokenizer
     *
     * @param reader Reader for the input
     */
    public Tokenizer(final java.io.Reader reader) {
        this.reader = reader;
        whitespace = new HashSet<>();
        symbols = new HashSet<>();

        line = 0;
        position = 0;

        buffer = new ArrayList<>(LOOKAHEAD_BUF_SIZE);
        tokenValue = new StringBuilder();

        setupForGeneralParsing();
    }

    /**
     * Returns value of token that was read from input during last call to {@link #nextToken()}
     *
     * @return value of read token
     */
    public String getTokenValue() {
        return tokenValue.toString();
    }

    /**
     * @return Current position in the current line in the input stream
     */
    public int getPosition() {
        return position;
    }

    /**
     * @return Current line in the input stream
     */
    public int getLine() {
        return line;
    }

    /**
     * Reads next token from the input stream. Ignores whitespace.
     *
     * @return Type of the token read from the input stream {@link Tokenizer.TextType}
     * @throws IOException If underlying reader caused an exception
     */
    public TextType nextToken() throws IOException {
        tokenValue.setLength(0);
        final int chr = skipWhitespace();
        if (EOF_MARK == chr) {
            return TextType.EOF;
        } else {
            pushBack((char) chr);
            return readWord();
        }
    }

    /**
     * Resets parsing rules for general parsing of LISP source code
     */
    public final void setupForGeneralParsing() {
        whitespace.clear();
        symbols.clear();

        whitespace.addAll(WHITESPACE);
        symbols.addAll(SYMBOLS);
    }

    /**
     * Resets parsing rules for parsing of strings
     */
    public final void setupForStringParsing() {
        setupForGeneralParsing();
        symbols.add('\\');
    }

    /**
     * Promote end of line to significant separator character instead of whitespace.
     */
    public final void eolIsSignificant() {
        whitespace.remove('\n');
        whitespace.remove('\r');

        symbols.add('\n');
        symbols.add('\r');
    }

    public boolean isFrontEqualTo(final String text) throws IOException {
        final String peeked = peek(text.length());
        if (peeked.equals(text)) {
            return true;
        } else {
            buffer.clear(); // ignore peeked text
            return false;
        }
    }

    void registerSpecial(final char specialCharacter) {
        symbols.add(specialCharacter);
    }

    private String peek(final int count) throws IOException {
        if (buffer.isEmpty()) {
            if (count > LOOKAHEAD_BUF_SIZE) {
                throw new IllegalArgumentException("peek count is greater than supported");
            }
            final int read = reader.read(cbuf);
            for (int i = 0; i < read; i++) {
                buffer.add(cbuf[i]);
            }
            return new String(cbuf, 0, read);
        } else {
            throw new IllegalStateException("Internal lookahead buffer is not empty, cannot peek");
        }
    }

    private TextType readWord() throws IOException {
        while (true) {
            final int chr = getChar();
            if (EOF_MARK == chr) {
                if (tokenValue.length() > 0) {
                    return TextType.WORD;
                } else {
                    return TextType.EOF;
                }
            }
            final char read = (char) chr;
            if (symbols.contains(read)) {
                if (tokenValue.length() == 0) {
                    tokenValue.append(read);
                    return TextType.SEPARATOR;
                } else {
                    pushBack(read);
                    return TextType.WORD;
                }
            } else {
                tokenValue.append(read);
            }
        }
    }

    private void pushBack(final char read) {
        if (buffer.isEmpty()) {
            buffer.add(read);
        } else {
            throw new IllegalStateException("Cannot push-back a character when buffer is not empty");
        }
    }

    private int skipWhitespace() throws IOException {
        int chr = getChar();
        while (chr >= 0 && whitespace.contains((char) chr)) {
            chr = getChar();
            if (chr == '\n') {
                line += 1;
                position = 0;
            } else {
                position += 1;
            }
        }
        return chr;
    }

    private int getChar() throws IOException {
        if (buffer.isEmpty()) {
            return reader.read();
        } else {
            return buffer.remove(0);
        }
    }

    /**
     * Supported types of tokens
     */
    public enum TextType {
        /**
         * End of stream token
         */
        EOF,
        /**
         * Separator token. Which character is treated as separator depends on current Tokenizer configuration
         */
        SEPARATOR,
        /**
         * Any word token. Numbers are words as well.
         */
        WORD
    }

    static {
        SYMBOLS.addAll(WHITESPACE);
        SYMBOLS.addAll(Collect.setOf(
                '(', ')', '[', ']', '{', '}', '"', ';', ','
        ));
    }

}
