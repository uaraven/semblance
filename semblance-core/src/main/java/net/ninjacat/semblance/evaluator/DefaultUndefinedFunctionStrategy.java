package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.FunctionExpectedException;

import javax.annotation.Nonnull;

/**
 * Default strategy to handle undefined functions. Throws {@link FunctionExpectedException}.
 */
public class DefaultUndefinedFunctionStrategy implements UndefinedFunctionStrategy {
    @Nonnull
    @Override
    public LispValue handle(@Nonnull final Context context,
                            @Nonnull final LispValue head,
                            @Nonnull final LispCollection params) {
        throw new FunctionExpectedException(head);
    }
}
