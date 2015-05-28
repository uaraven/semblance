package net.ninjacat.semblance;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.reader.Reader;
import net.ninjacat.semblance.utils.Values;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Semblance interpreter.
 */
public class Interpreter {

    private final RootContext rootContext;

    /**
     * Creates new interpreter. This sets up internal interpreter context which will be shared
     * by all scripts executed by this interpreter.
     */
    public Interpreter() {
        rootContext = new RootContext();
    }

    /**
     * Runs script from an input stream.
     *
     * @param stream Script source.
     * @return Value returned from script.
     * @throws ParsingException                                                In case of syntax error.
     * @throws net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue run(final InputStream stream) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.read(stream);
        return doRun(program);
    }

    /**
     * Runs script from an string.
     *
     * @param text Script source.
     * @return Value returned from script.
     * @throws ParsingException                                                In case of syntax error.
     * @throws net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue run(final String text) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.readString(text);
        return doRun(program);
    }

    /**
     * Compiles a Semblance program to a stream.
     *
     * @param source      Source stream
     * @param destination Stream to write compiled program to
     * @throws IOException      If source file cannot be read, or destination cannot be written to
     * @throws ParsingException If source has syntax errors
     */
    public void compile(final InputStream source, final OutputStream destination) throws IOException, ParsingException {
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
    public void compile(final File source, final String destinationFolder) throws IOException, ParsingException {
        try (InputStream input = new FileInputStream(source);
             OutputStream output = new FileOutputStream(getDestinationFileName(source, destinationFolder).toFile())) {
            compile(input, output);
        }
    }

    /**
     * Runs script from a binary file.
     *
     * @param fileName Name of file containing compiled program.
     * @return Value returned from script.
     * @throws ParsingException                                                In case of syntax error.
     * @throws net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue runCompiledFile(final String fileName) throws ParsingException {
        try (
                final FileInputStream fis = new FileInputStream(fileName);
                final ObjectInputStream dis = new ObjectInputStream(fis)) {
            final SList program = Values.asSList((LispValue) dis.readObject());
            return doRun(program);
        } catch (final Exception e) {
            throw new ParsingException("Failed to load compiled file " + fileName, e, SourceInfo.UNKNOWN);
        }
    }

    /**
     * @return This interpreter's root context.
     */
    public Context getRootContext() {
        return rootContext;
    }

    private static Path getDestinationFileName(final File source, final String destinationFolder) {
        final Path sourceFileName = Paths.get(source.getAbsolutePath()).getFileName();
        final String destFileName = sourceFileName.getName(0).toString();
        return Paths.get(destinationFolder, destFileName, "sc");
    }

    private LispValue doRun(final SList program) {
        return rootContext.evaluateProgram(program);
    }

}
