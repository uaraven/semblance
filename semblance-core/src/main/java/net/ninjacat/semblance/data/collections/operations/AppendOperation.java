package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.isCollection;

/**
 * Append operation
 */
public class AppendOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        if (parameters.length() == 1 && isCollection(parameters.head())) {
            return source.append(asCollection(parameters.head()));
        } else {
            return source.append(parameters);
        }
    }
}
