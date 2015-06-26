package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;

/**
 * Takes first n elements of collection
 */
public class TakeOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final int count = (int) Values.getLongValue(parameters.head());
        return source.slice(0, count - 1);
    }
}
