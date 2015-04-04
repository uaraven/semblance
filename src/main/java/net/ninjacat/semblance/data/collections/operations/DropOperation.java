package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * Takes all but first n elements of collection
 */
public class DropOperation implements ListOperation {
    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        final int count = (int) Values.getLongValue(parameters.head());
        return source.slice(count, source.length() - 1);
    }
}
