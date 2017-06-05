package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

/**
 * Takes last element of collection
 */
public class LastOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        return source.get(source.length() - 1);
    }
}
