package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Pauses current thread for a given amount of milliseconds.
 * Returns T if delay completed ok, or F if it was interrupted
 */
public class Delay extends BuiltInFunction {

    /**
     * Creates a new instance of Delay
     */
    public Delay() {
        super("delay", "milliseconds");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final long delay = asNumber(evaluated.head()).longValue();
        try {
            Thread.sleep(delay);
            return Constants.TRUE;
        } catch (final InterruptedException e) {
            return Constants.FALSE;
        }
    }
}
