package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Takes last element of collection
 */
public class LastOperation implements ListOperation {
    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        return source.get(source.length() - 1);
    }
}
