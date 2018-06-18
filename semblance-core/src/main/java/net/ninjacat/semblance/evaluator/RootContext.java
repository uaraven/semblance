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
import net.ninjacat.semblance.builtin.spforms.types.AsDouble;
import net.ninjacat.semblance.builtin.spforms.types.AsInt;
import net.ninjacat.semblance.builtin.spforms.types.AsString;
import net.ninjacat.semblance.builtin.spforms.types.ParseNumber;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.data.special.WrappedValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static net.ninjacat.semblance.evaluator.SourceUtils.readProgram;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Base context for every program.
 * <p/>
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
        super(symbol("/"), null, Optional.of(undefinedFunctionStrategy));

        bind(symbol("nil"), NilCollection.INSTANCE);
        bind(symbol("T"), Constants.TRUE);
        bind(symbol("F"), Constants.FALSE);

        bindSpecialForms();
        loadLibrary();

        sourceFolders = new ArrayList<>();
    }

    /**
     * Evaluates program in supplied context. Used by REPL
     *
     * @param program  The program
     * @param context Execution context
     * @return evaluated value
     */
    public static LispValue evaluateInContext(final LispCollection program,
                                              final Context context) {
        return unwrapWrappers(context.evaluateBlock(program));
    }

    /**
     * Creates a new local context and evaluates a program in that context.
     * <p>
     * Any bindings made by the program are not persisted in this root context.
     *
     * @param program InputStream containing program.
     * @return Value evaluated.
     */
    public LispValue evaluateProgram(final LispCollection program) {
        final Context executionContext = LocalContext.namedChildContext(symbol("main"), this);
        return unwrapWrappers(executionContext.evaluateBlock(program));
    }

    /**
     * Evaluates program in this context, all binding done in the program are persisted in this root context.
     *
     * @param program The program.
     * @return Result of program evaluation.
     */
    public LispValue evaluateHere(final SList program) {
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

        bindCallable(new WriteOut());
        bindCallable(new WriteOutCr());

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
        bindCallable(new Cond());
        bindCallable(new Select());
        bindCallable(new Equal());
        bindCallable(new NotEqual());
        bindCallable(new GreaterThan());
        bindCallable(new GreaterEqual());
        bindCallable(new LessThan());
        bindCallable(new LessEqual());
        bindCallable(new And());
        bindCallable(new Not());
        bindCallable(new Or());

        bindCallable(new AsString());
        bindCallable(new AsDouble());
        bindCallable(new AsInt());
        bindCallable(new ParseNumber());

        bindCallable(new Find());

        bindCallable(new MapKeys());
        bindCallable(new MapValues());
        bindCallable(new MapGet());

        bindCallable(new Contains());
        bindCallable(new Zip());
    }

    private void prepareDefaultNamespaces() {
        addNamespace(new BaseNamespace(symbol("list")));
        addNamespace(new BaseNamespace(symbol("map")));
    }

    private void loadLibrary() {
        try (InputStream source = getClass().getResourceAsStream("/standard.smbl")) {
            evaluateHere(readProgram(source));
        } catch (ParsingException | IOException e) {
            throw new SemblanceRuntimeException("Failed to load standard library", SourceInfo.UNKNOWN, e);
        }
    }

    private void bindCallable(final LispCallable form) {
        bind(form.name(), form);
    }
}
