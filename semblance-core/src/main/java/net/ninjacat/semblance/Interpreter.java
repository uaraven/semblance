package net.ninjacat.semblance;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.ContextModifier;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.reader.Reader;
import net.ninjacat.semblance.utils.Values;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;

import static net.ninjacat.semblance.utils.Values.list;

/**
 * Semblance interpreter.
 */
public class Interpreter {

    private final RootContext rootContext;

    /**
     * Creates a new interpreter with a supplied root context modifiers. Allows to bind functions, change namespaces, etc.
     *
     * @param contextModifiers Root context modifiers.
     */
    public Interpreter(final ContextModifier... contextModifiers) {
        rootContext = new RootContext();
        for (final ContextModifier modifier : contextModifiers) {
            modifier.modify(rootContext);
        }
    }

    /**
     * Modifies root context of this interpreter with a given context modifier
     *
     * @param contextModifier Context modifier for interpreter's root context
     */
    public void injectContextModifier(final ContextModifier contextModifier) {
        contextModifier.modify(rootContext);
    }

    /**
     * Runs script from an input stream.
     *
     * @param stream Script source.
     * @return Value returned from script.
     * @throws ParsingException          In case of syntax error.
     * @throws SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue run(final InputStream stream) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.read(stream);
        return doRun(program);
    }

    /**
     * Runs script from an string. New context is created for the program to run in, so none of bound symbols affects
     * interpreter.
     *
     * @param text Script source.
     * @return Value returned from script.
     * @throws ParsingException          In case of syntax error.
     * @throws SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue run(final String text) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.readString(text);
        return doRun(program);
    }

    /**
     * Runs parsed program
     *
     * @param program {@link SList} containing parsed program
     * @return Value returned from string
     * @throws SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue runProgram(final SList program) {
        return doRun(program);
    }

    /**
     * Runs script from a binary file.
     *
     * @param fileName Name of file containing compiled program.
     * @return Value returned from script.
     * @throws ParsingException          In case of syntax error.
     * @throws SemblanceRuntimeException In case of runtime exception.
     */
    public LispValue runCompiledFile(final String fileName) throws ParsingException {
        try (final FileInputStream fis = new FileInputStream(fileName);
             final ObjectInputStream dis = new ObjectInputStream(fis)) {
            final SList program = Values.asSList((LispValue) dis.readObject());
            return doRun(program);
        } catch (final Exception e) {
            throw new ParsingException("Failed to load compiled file " + fileName, e, SourceInfo.UNKNOWN);
        }
    }

    /**
     * Shortcut to call a single function, may be useful for embedding
     *
     * @param functionName Name of the function to call
     * @param parameters   function parameters
     * @return function result
     */
    public LispValue callFunction(final SymbolAtom functionName, final SList parameters) {
        final LispCollection call = list(functionName).append(parameters);
        return rootContext.evaluateProgram(list(call));
    }

    /**
     * Executes supplied program in root context of this interpreter. All bindings are stored and available on the
     * next call of {@link #run(String)}method.
     *
     * @param program Source of the program to execute
     * @return Result of the evaluation
     * @throws ParsingException If source contains errors.
     */
    public LispValue runHere(final String program) throws ParsingException {
        final Reader reader = new Reader();
        final SList parsed = reader.readString(program);
        return rootContext.evaluateHere(parsed);
    }

    /**
     * @return This interpreter's root context.
     */
    public Context getRootContext() {
        return rootContext;
    }

    private LispValue doRun(final SList program) {
        return rootContext.evaluateProgram(program);
    }
}
