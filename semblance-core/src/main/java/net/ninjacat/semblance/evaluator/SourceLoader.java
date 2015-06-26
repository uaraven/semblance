package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.compile.ParsingIOException;
import net.ninjacat.semblance.reader.Reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;

/**
 * Utility class providing tools to read source and compiled semblance files.
 */
public final class SourceLoader {
    private SourceLoader() {
    }

    /**
     * Reads program's source code from {@link InputStream} and parses it.
     *
     * @param source Input stream containing program source
     * @return {@link SList} representing program
     * @throws ParsingException In the event of syntax error found during parsing
     */
    public static SList readProgram(final InputStream source) throws ParsingException {
        final Reader reader = new Reader();
        return reader.read(source);
    }

    /**
     * Parses program from a string.
     *
     * @param source String containing source code.
     * @return {@link SList} representing program
     * @throws ParsingException In the event of syntax error found during parsing
     */
    public static SList readProgram(final String source) throws ParsingException {
        final Reader reader = new Reader();
        return reader.readString(source);
    }

    /**
     * Reads compiled program from a stream.
     *
     * @param compiled Input stream containing compiled program representation.
     * @return {@link SList} representing program
     * @throws ParsingException In the event of missing classes or IO exception.
     */
    public static SList readCompiled(final InputStream compiled) throws ParsingException {
        try (final ObjectInputStream programStream = new ObjectInputStream(compiled)) {
            return (SList) programStream.readObject();
        } catch (final ClassNotFoundException | IOException e) {
            throw new ParsingIOException("Cannot load compiled program", e);
        }
    }

}
