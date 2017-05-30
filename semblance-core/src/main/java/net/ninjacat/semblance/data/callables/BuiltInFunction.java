package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.builtin.spforms.CreatesContext;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import javax.annotation.Nonnull;

/**
 * Base class for built-in functions. This wrapper always evaluates passed parameters before passing control to actual
 * implementation
 */
@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
@CreatesContext
public abstract class BuiltInFunction extends ParametrizedCallable {

    private static final long serialVersionUID = -5282625835226066711L;

    /**
     * Creates new instance of the built-in funtion.
     *
     * @param definition Definition of built-in function.
     */
    protected BuiltInFunction(final String... definition) {
        super(definition);
    }

    @Override
    public final LispValue apply(final Context context, final LispCollection parameters) {
        return applyFunction(LocalContext.namedChildContext(name(), context), context.evaluateList(parameters));
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        //noinspection ObjectEquality
        if (other == this) {
            return 0;
        } else {
            throw new ClassCastException(String.format("%s is not compatible with %s", getType(), other.getType()));
        }
    }

    protected abstract LispValue applyFunction(final Context context, final LispCollection evaluated);

}
