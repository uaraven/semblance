package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;

import javax.annotation.Nonnull;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Filter operation on a list.
 */
public class FilterOperation implements ListOperation {
    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final Callable predicate = asCallable(parameters.head());

        final List<LispValue> results = Iter.of(source.getCollection()).filter(new Predicate<LispValue>() {
            @Override
            public boolean matches(final LispValue item) {
                return isTrue(predicate.apply(context, list(item)));
            }
        }).toList();

        return source.createNew(results);
    }
}
