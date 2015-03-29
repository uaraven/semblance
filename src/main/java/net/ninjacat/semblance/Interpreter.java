package net.ninjacat.semblance;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.reader.Reader;
import net.ninjacat.semblance.utils.Values;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;

import static net.ninjacat.semblance.utils.Values.symbol;

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

    private LispValue doRun(final SList program) {
        final Context executionContext = LocalContext.namedChildContext(symbol("main"), rootContext);
        return executionContext.evaluateBlock(program);
    }

}
