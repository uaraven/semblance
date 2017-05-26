package net.ninjacat.semblance.concurrent.func;

import net.ninjacat.semblance.concurrent.data.SFuture;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

/**
 * Executes supplied code block asynchronously and returns a future
 */
public class AsyncRun extends SpecialForm {

    private static final long serialVersionUID = 3017617937876904883L;

    /**
     * Creates and async special form
     */
    public AsyncRun() {
        super("run", "&rest", "code-block");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namelessChildContext(context);
        return new SFuture(localContext, parameters);
    }
}
