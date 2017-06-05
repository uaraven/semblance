package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.compile.ParsingIOException;
import net.ninjacat.semblance.reader.Reader;

import java.io.*;

/**
 * Utility class providing tools to read source and compiled semblance files.
 */
public final class SourceUtils {
    private SourceUtils() {
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

    /**
     * Loads program from stream and compiles it into another stream.
     * This method will not close neither source nor destination streams.
     *
     * @param source InputStream with program source.
     * @param dest   OutputStream to receive binary representation of a program.
     * @return Lisp collection representing a program.
     * @throws ParsingException if program cannot be compiled.
     */
    public static LispValue compileToStream(final InputStream source, final OutputStream dest) throws ParsingException {
        final SList program = readProgram(source);

        try (final ObjectOutputStream outputStream = new ObjectOutputStream(dest)) {
            outputStream.writeObject(program);
            outputStream.flush();
        } catch (final IOException e) {
            throw new ParsingException("Failed to save program", e, SourceInfo.UNKNOWN);
        }
        return program;
    }
}
