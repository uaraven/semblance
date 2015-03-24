package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.builtin.spforms.*;
import net.ninjacat.semblance.builtin.spforms.Namespace;
import net.ninjacat.semblance.builtin.spforms.arithmetic.*;
import net.ninjacat.semblance.builtin.spforms.comparison.*;
import net.ninjacat.semblance.builtin.spforms.logic.And;
import net.ninjacat.semblance.builtin.spforms.logic.Not;
import net.ninjacat.semblance.builtin.spforms.logic.Or;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.reader.Reader;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Base context for every program.
 * <p/>
 * Created on 01/03/15.
 */
public class RootContext extends BaseContext {
    /**
     * Creates new instance of root context
     */
    public RootContext() {
        super("/", null);

        bind(symbol("nil"), NilCollection.INSTANCE);
        bind(symbol("T"), Constants.TRUE);
        bind(symbol("F"), Constants.FALSE);

        bindSpecialForms();
    }

    /**
     * Evaluates program in the this context. Any bindings made by the program are stored in the context.
     *
     * @param source InputStream containing program.
     * @return Value evaluated.
     * @throws ParsingException If program contains syntax errors.
     */
    public LispValue evaluateProgram(final InputStream source) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.read(source);
        return evaluateBlock(program);
    }

    /**
     * Loads program from stream and compiles it into another stream.
     * This method will not close neigher source nor destination streams.
     *
     * @param source InputStream with program source.
     * @param dest   OutputStream to receive binary representation of a program.
     * @return Lisp collection representing a program.
     * @throws ParsingException if program cannot be compiled.
     */
    public LispValue compile(final InputStream source, final OutputStream dest) throws ParsingException {
        final Reader reader = new Reader();
        final SList program = reader.read(source);

        try (final ObjectOutputStream outputStream = new ObjectOutputStream(dest)) {
            outputStream.writeObject(program);
            outputStream.flush();
        } catch (final IOException e) {
            throw new ParsingException("Failed to save program", e, SourceInfo.UNKNOWN);
        }
        return program;
    }

    @Override
    protected Context createChild(final String name) {
        return LocalContext.namedChildContext(name, this);
    }

    private void bindSpecialForms() {
        bindForm(new Var());

        bindForm(new EvalMe());
        bindForm(new UnwrapMe());

        bindForm(new Quote());
        bindForm(new BackQuote());
        bindForm(new PrintLn());

        bindForm(new Fn());
        bindForm(new Defmacro());

        bindForm(new Add());
        bindForm(new Sub());
        bindForm(new Div());
        bindForm(new Mul());
        bindForm(new Mod());

        bindForm(new Namespace());
        bindForm(new Progn());
        bindForm(new Let());

        bindForm(new If());
        bindForm(new Equal());
        bindForm(new NotEqual());
        bindForm(new GreaterThan());
        bindForm(new GreaterEqual());
        bindForm(new LessThan());
        bindForm(new LessEqual());
        bindForm(new And());
        bindForm(new Not());
        bindForm(new Or());

        loadLibrary();
    }

    private void loadLibrary() {
        try (InputStream source = getClass().getResourceAsStream("/standard.smbl")) {
            evaluateProgram(source);
        } catch (ParsingException | IOException e) {
            throw new SemblanceRuntimeException("Failed to load standard library", SourceInfo.UNKNOWN, e);
        }
    }

    private void bindForm(final SpecialForm form) {
        bind(form.name(), form);
    }
}
