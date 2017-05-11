package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.stream.Collectors;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Filter operation on a list.
 */
public class FilterOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final LispCallable predicate = asCallable(parameters.head());

        final List<LispValue> results = source.stream().filter(item ->
                isTrue(predicate.apply(context, list(item)))).collect(Collectors.toList());

        return source.createNew(results);
    }
}
