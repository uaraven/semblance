package net.ninjacat.semblance;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.reader.Reader;

import java.io.InputStream;

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

    private LispValue doRun(final SList program) {
        final Context executionContext = LocalContext.namedChildContext("main", rootContext);
        LispValue evaluated = NilCollection.INSTANCE;
        for (final LispValue sExpression : program) {
            evaluated = executionContext.evaluate(sExpression);
        }
        return evaluated;
    }

}
