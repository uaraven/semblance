package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

import java.util.Collections;

/**
 * Sorts collection
 */
public class SortOperation implements ListOperation {

    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        final LispCollection result = source.createNew(source.getCollection());

        Collections.sort(result.getCollection());

        return result;
    }
}
