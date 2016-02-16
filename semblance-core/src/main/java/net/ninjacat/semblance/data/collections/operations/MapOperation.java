package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

import static net.ninjacat.semblance.utils.Values.asCallable;

/**
 * Functional map operations on collections
 */
public class MapOperation implements ListOperation {

    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final LispCallable applicator = asCallable(parameters.head());

        return source.foreach(context, applicator);
    }
}
