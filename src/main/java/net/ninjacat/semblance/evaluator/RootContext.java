package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.builtin.spforms.*;
import net.ninjacat.semblance.builtin.spforms.Namespace;
import net.ninjacat.semblance.builtin.spforms.arithmetic.*;
import net.ninjacat.semblance.builtin.spforms.collections.Contains;
import net.ninjacat.semblance.builtin.spforms.collections.Find;
import net.ninjacat.semblance.builtin.spforms.collections.MapKeys;
import net.ninjacat.semblance.builtin.spforms.collections.MapValues;
import net.ninjacat.semblance.builtin.spforms.comparison.*;
import net.ninjacat.semblance.builtin.spforms.logic.And;
import net.ninjacat.semblance.builtin.spforms.logic.Not;
import net.ninjacat.semblance.builtin.spforms.logic.Or;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
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
        super(symbol("/"), null);

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
     * Evaluates program in the this context. Any bindings made by the program are stored in the context.
     *
     * @param source InputStream containing program.
     * @return Value evaluated.
     * @throws ParsingException If program contains syntax errors.
     */
    public LispValue evaluateProgram(final LispCollection source) throws ParsingException {
        return evaluateBlock(source);
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
    public LispValue compileToStream(final InputStream source, final OutputStream dest) throws ParsingException {
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

    private void bindSpecialForms() {
        bindForm(new Var());

        bindForm(new EvalMe());
        bindForm(new UnwrapMe());

        bindForm(new Quote());
        bindForm(new Backquote());
        bindForm(new PrintLn());

        bindForm(new Namespace());
        bindForm(new Progn());
        bindForm(new Block());
        bindForm(new Fn());
        bindForm(new Defmacro());
        bindForm(new Funcall());
        bindForm(new Return());
        bindForm(new Loop());
        bindForm(new Break());

        bindForm(new Add());
        bindForm(new Sub());
        bindForm(new Div());
        bindForm(new Mul());
        bindForm(new Mod());

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

        bindForm(new Contains());
        bindForm(new Find());
        bindForm(new MapKeys());
        bindForm(new MapValues());

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
