package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Func;

/**
 * {@link LispCallable} bound to an execution {@link Context}. This class is to simplify calling Lisp functions from
 * Java
 */
public class BoundLispCallable implements Func<LispValue, LispCollection> {

    private final Context context;
    private final LispCallable callable;

    public BoundLispCallable(final Context context, final LispCallable callable) {
        this.context = context;
        this.callable = callable;
    }

    @Override
    public LispValue apply(final LispCollection parameters) {
        return this.callable.apply(this.context, parameters);
    }
}
