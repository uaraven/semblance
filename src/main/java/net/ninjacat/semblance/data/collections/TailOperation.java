package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;

/**
 * head operation on a collection
 */
public class TailOperation implements ListOperation {
    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        return source.tail();
    }
}
