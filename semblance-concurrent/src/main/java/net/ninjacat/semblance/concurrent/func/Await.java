package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asOpaque;
import static net.ninjacat.semblance.utils.Values.isOpaque;

/**
 * Awaits for a future completion, returns future result.
 */
public class Await extends BuiltInFunction {

    private static final long serialVersionUID = 3216055294812227317L;

    /**
     * Constructor
     */
    public Await() {
        super("await", "future");
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
            return future.getValue().getResult().getValue();
        } else {
            return parameter;
        }
    }
}
