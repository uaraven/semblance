package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.reader.macros.BackquoteMacro;
import net.ninjacat.semblance.reader.macros.QuoteMacro;

import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;

/**
 * Source code reader. Produces list of S-expressions as a result.
 */
public class Reader {

    private final Set<ReaderMacro> macros;

    /**
     * Create a reader, used to read and parse semblance source code.
     */
    public Reader() {
        macros = new HashSet<>();
        macros.add(new QuoteMacro());
        macros.add(new BackquoteMacro());
    }

    /**
     * Reads source from a stream and parses it into a {@link SList} of S-expressions.
     *
     * @param stream Source input.
     * @return List of S-expressions.
     * @throws ParsingException In case of syntax error.
     */
    public SList read(final InputStream stream) throws ParsingException {
        final ReaderStream readerStream = ReaderStream.readStream(stream);
        try {
            return parse(readerStream);
        } finally {
            readerStream.close();
        }
    }

    /**
     * Reads source from a string and parses it into a {@link SList} of S-expressions.
     *
     * @param text Source input.
     * @return List of S-expressions.
     * @throws ParsingException In case of syntax error.
     */
    public SList readString(final String text) throws ParsingException {
        final ReaderStream readerStream = ReaderStream.readString(text);
        try {
            return parse(readerStream);
        } finally {
            readerStream.close();
        }
    }

    private SList parse(final ReaderStream readerStream) throws ParsingException {
        final Parser parser = new Parser();

        registerMacros(readerStream, parser);

        return parser.parse(readerStream.tokenize());
    }

    private void registerMacros(final ReaderStream readerStream, final Parser parser) {
        for (final ReaderMacro macro : macros) {
            readerStream.registerSpecial(macro.getMacroCharacter().charAt(0));
            parser.registerReaderMacro(macro);
        }
    }
}
