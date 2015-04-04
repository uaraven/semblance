package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * head operation on a collection
 */
public class HeadOperation implements ListOperation {
    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        return source.head();
    }
}
