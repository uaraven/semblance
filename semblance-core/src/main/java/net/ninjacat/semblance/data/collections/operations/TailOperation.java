package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

/**
 * head operation on a collection
 */
public class TailOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        return source.tail();
    }
}
