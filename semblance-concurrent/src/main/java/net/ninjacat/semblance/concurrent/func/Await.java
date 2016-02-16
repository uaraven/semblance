package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asOpaque;

/**
 * Awaits for a future completion, returns future result
 */
public class Await extends BuiltInFunction {

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
        final SFuture future = (SFuture) asOpaque(evaluated.head());
        return future.getValue().getResult().getValue();
    }
}
