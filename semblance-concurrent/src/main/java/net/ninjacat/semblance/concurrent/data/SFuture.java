package net.ninjacat.semblance.concurrent.data;

import net.ninjacat.semblance.concurrent.futures.Future;
import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

/**
 * Future for Semblance.
 */
public class SFuture extends OpaqueValue<Future<LispValue>> {

    private static final long serialVersionUID = 3647681801341085399L;

    /**
     * Creates a new SFuture and runs supplied code in a supplied context
     *
     * @param context Context to run code in
     * @param code    Code to execute
     */
    public SFuture(@Nonnull final Context context, @Nonnull final LispCollection code) {
        super(Future.run(() -> context.evaluateBlock(code)));
    }

    /**
     * Creates a new SFuture and executes a supplied function with parameters in a provided context
     *
     * @param context  Context to run code in
     * @param function Function (or any other {@link LispCallable}) to execute
     * @param params   Parameters for the callable
     */
    public SFuture(@Nonnull final Context context, @Nonnull final LispCallable function, @Nonnull final LispCollection params) {
        super(Future.run(() -> function.apply(context, params)));
    }
}
