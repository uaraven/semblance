package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

import static net.ninjacat.semblance.utils.Values.asOpaque;
import static net.ninjacat.semblance.utils.Values.isOpaque;

/**
 * Checks for a future completion, returns {@code T} or {@code F}
 */
public class Check extends BuiltInFunction {

    private static final long serialVersionUID = 4954008329365167831L;

    /**
     * Constructor for check function
     */
    public Check() {
        super("check", "future");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        if (evaluated.length() < 1) {
            throw new SemblanceRuntimeException("Future expected", evaluated.getSourceInfo());
        }
        final LispValue parameter = evaluated.head();
        if (isOpaque(parameter) && parameter instanceof SFuture) {
            final OpaqueValue<?> opaqueValue = asOpaque(parameter);
            final SFuture future = (SFuture) opaqueValue;
            return future.getValue().isCompleted() ? Values.T : Values.F;
        } else {
            return Values.T;
        }
    }
}
