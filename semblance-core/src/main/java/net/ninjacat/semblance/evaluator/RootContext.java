package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.Update;
import net.ninjacat.semblance.builtin.lib.collections.*;
import net.ninjacat.semblance.builtin.spforms.*;
import net.ninjacat.semblance.builtin.spforms.Namespace;
import net.ninjacat.semblance.builtin.spforms.arithmetic.*;
import net.ninjacat.semblance.builtin.spforms.comparison.*;
import net.ninjacat.semblance.builtin.spforms.logic.And;
import net.ninjacat.semblance.builtin.spforms.logic.Not;
import net.ninjacat.semblance.builtin.spforms.logic.Or;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.special.WrappedValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.smooth.utils.Option;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static net.ninjacat.semblance.evaluator.SourceLoader.readProgram;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Base context for every program.
 * <p/>
 * Created on 01/03/15.
 */
public class RootContext extends BaseContext {

    private final List<String> sourceFolders;

    /**
     * Creates new instance of root context
     */
    public RootContext() {
        this(new DefaultUndefinedFunctionStrategy());
    }

    /**
     * Creates new instnace of root context with custom {@link UndefinedFunctionStrategy}
     *
     * @param undefinedFunctionStrategy Strategy to resolve undefined functions
     */
    public RootContext(@Nonnull final UndefinedFunctionStrategy undefinedFunctionStrategy) {
        super(symbol("/"), null, Option.of(undefinedFunctionStrategy));

        bind(symbol("nil"), NilCollection.INSTANCE);
        bind(symbol("T"), Constants.TRUE);
        bind(symbol("F"), Constants.FALSE);

        bindSpecialForms();
        sourceFolders = new ArrayList<>();
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

    /**
     * Evaluates program in supplied context. Used by REPL
     *
     * @param source  Program source
     * @param context Execution context
     * @return evaluated value
     * @throws ParsingException If syntax error is encountered during evaluation.
     */
    public static LispValue evaluateInContext(final String source, final Context context) throws ParsingException {
        return context.evaluateBlock(readProgram(source));
    }

    /**
     * Evaluates program in the this context. Any bindings made by the program are not stored in the context.
     *
     * @param source InputStream containing program.
     * @return Value evaluated.
     * @throws ParsingException If program contains syntax errors.
     */
    public LispValue evaluateProgram(final InputStream source) throws ParsingException {
        final SList program = readProgram(source);
        final Context executionContext = LocalContext.namedChildContext(symbol("main"), this);
        return unwrapWrappers(executionContext.evaluateBlock(program));
    }

    /**
     * Evaluates program in the this context. Any bindings made by the program are not stored in the context.
     *
     * @param source InputStream containing program.
     * @return Value evaluated.
     * @throws ParsingException If program contains syntax errors.
     */
    public LispValue evaluateCompiledProgram(final InputStream source) throws ParsingException {
        final SList program = SourceLoader.readCompiled(source);
        final Context executionContext = LocalContext.namedChildContext(symbol("main"), this);
        return unwrapWrappers(executionContext.evaluateBlock(program));
    }

    /**
     * Evaluates program in the this context. Any bindings made by the program are not stored in the context.
     *
     * @param source InputStream containing program.
     * @return Value evaluated.
     */
    public LispValue evaluateProgram(final LispCollection source) {
        final Context executionContext = LocalContext.namedChildContext(symbol("main"), this);
        return unwrapWrappers(executionContext.evaluateBlock(source));
    }

    /**
     * Evaluates source in this context, all binding done in the program are stored in this root context indefinitely.
     *
     * @param source Program source.
     * @return Result of program evaluation.
     * @throws ParsingException If program contains source errors.
     */
    public LispValue evaluateHere(final InputStream source) throws ParsingException {
        final SList program = readProgram(source);
        return unwrapWrappers(evaluateBlock(program));
    }

    @Override
    public List<String> getSourceFolders() {
        return Collections.unmodifiableList(sourceFolders);
    }

    @Override
    public void setSourceFolders(final List<String> sourceFolders) {
        this.sourceFolders.clear();
        this.sourceFolders.addAll(sourceFolders);
    }

    private static LispValue unwrapWrappers(final LispValue value) {
        return value instanceof WrappedValue ? ((WrappedValue) value).getValue() : value;
    }

    private void bindSpecialForms() {
        prepareDefaultNamespaces();

        bindCallable(new Include());

        bindCallable(new Var());
        bindCallable(new Update());
        bindCallable(new net.ninjacat.semblance.builtin.lib.collections.List());

        bindCallable(new EvalMe());
        bindCallable(new UnwrapMe());

        bindCallable(new Quote());
        bindCallable(new Backquote());
        bindCallable(new PrintLn());

        bindCallable(new Namespace());
        bindCallable(new Use());
        bindCallable(new Progn());
        bindCallable(new Block());
        bindCallable(new Fn());
        bindCallable(new Defmacro());
        bindCallable(new Funcall());
        bindCallable(new Return());
        bindCallable(new Loop());
        bindCallable(new Break());
        bindCallable(new Recur());

        bindCallable(new Add());
        bindCallable(new Sub());
        bindCallable(new Div());
        bindCallable(new Mul());
        bindCallable(new Mod());

        bindCallable(new If());
        bindCallable(new Equal());
        bindCallable(new NotEqual());
        bindCallable(new GreaterThan());
        bindCallable(new GreaterEqual());
        bindCallable(new LessThan());
        bindCallable(new LessEqual());
        bindCallable(new And());
        bindCallable(new Not());
        bindCallable(new Or());

        bindCallable(new Find());
        bindCallable(new MapKeys());
        bindCallable(new MapValues());

        bindCallable(new Contains());
        bindCallable(new Zip());

        loadLibrary();
    }

    private void prepareDefaultNamespaces() {
        addNamespace(new BaseNamespace(symbol("list")));
        addNamespace(new BaseNamespace(symbol("map")));
    }

    private void loadLibrary() {
        try (InputStream source = getClass().getResourceAsStream("/standard.smbl")) {
            evaluateHere(source);
        } catch (ParsingException | IOException e) {
            throw new SemblanceRuntimeException("Failed to load standard library", SourceInfo.UNKNOWN, e);
        }
    }

    private void bindCallable(final LispCallable form) {
        bind(form.name(), form);
    }
}
