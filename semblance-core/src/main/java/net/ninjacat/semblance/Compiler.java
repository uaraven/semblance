package net.ninjacat.semblance;

import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.reader.Reader;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * "Compiler" for Semblance.
 * <p>
 * Provides methods to convert source code into serializable SList
 */
public final class Compiler {

    private static final String COMPILED_EXT = ".smc";

    private Compiler() {
    }

    private static Path getDestinationFileName(final File source, final String destinationFolder) {
        final Path sourceFileName = Paths.get(source.getAbsolutePath()).getFileName();
        final String destFileName = sourceFileName.getName(0).toString();
        final int extPos = destFileName.lastIndexOf('.');
        final String destName = (extPos > 0 ? destFileName.substring(0, extPos) : destFileName) + COMPILED_EXT;
        return Paths.get(destinationFolder, destName);
    }

    /**
     * Reads source code from InputStream and converts it to list of S-expressions
     *
     * @param source {@link InputStream} with source code
     * @return {@link SList} with S-expression
     * @throws ParsingException In case of syntax errors
     */
    public static SList read(final InputStream source) throws ParsingException {
        final Reader reader = new Reader();
        return reader.read(source);
    }

    /**
     * Reads source code from String and converts it to list of S-expressions
     *
     * @param source Source code
     * @return {@link SList} with S-expression
     * @throws ParsingException In case of syntax errors
     */
    public static SList read(final String source) throws ParsingException {
        final Reader reader = new Reader();
        return reader.readString(source);
    }


    /**
     * Compiles a Semblance program to a stream.
     *
     * @param source      Source stream
     * @param destination Stream to write compiled program to
     * @throws IOException      If source file cannot be read, or destination cannot be written to
     * @throws ParsingException If source has syntax errors
     */
    public static void compile(final InputStream source, final OutputStream destination) throws IOException, ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.read(source);
        try (ObjectOutputStream oos = new ObjectOutputStream(destination)) {
            oos.writeObject(program);
        }
    }

    /**
     * Compiles a Semblance program.
     * Will create new file in destination folder with the same name as source file and .sc extension
     *
     * @param source            Source file
     * @param destinationFolder Folder where to put compiled file
     * @throws IOException      If source file cannot be read, or destination cannot be written to
     * @throws ParsingException If source has syntax errors
     */
    public static void compile(final File source, final String destinationFolder) throws IOException, ParsingException {
        try (InputStream input = new FileInputStream(source);
             OutputStream output = new FileOutputStream(getDestinationFileName(source, destinationFolder).toFile())) {
            compile(input, output);
        }
    }
}
