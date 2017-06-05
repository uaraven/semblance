package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * Runs supplied function in parallel (if possible, not guaranteed)
 * Returns future result.
 * <p>
 * Function will be resolved before spawning a separate thread, but all parameters will be evaluated in the spawned
 * thread. In case of failure to evaluate any of the parameters the exception will most
 * probably not be thrown immediately.
 * <p>
 * (go add (1 2))
 */
public class GoFunc extends SpecialForm {

    private static final long serialVersionUID = 2249021754950543209L;

    /**
     * Creates a new instance of go special form
     */
    public GoFunc() {
        super("go", "function", "(parameters)");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCallable func = Values.asCallable(context.evaluate(parameters.head()));
        final LispCollection evaluatedParams = context.evaluateList(asCollection(parameters.tail().head()));
        return new SFuture(context, func, evaluatedParams);
    }
}
