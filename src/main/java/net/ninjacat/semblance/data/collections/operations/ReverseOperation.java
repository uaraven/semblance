package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

import java.util.List;

/**
 * Reverses collection
 */
public class ReverseOperation implements ListOperation {

    @Override
    public LispValue apply(final LispCollection source, final LispCollection parameters) {
        final LispCollection result = source.createNew(source.getCollection());
        final List<LispValue> data = result.getCollection();
        for (int i = 0; i < data.size() / 2; i++) {
            swap(data, i, data.size() - i - 1);
        }
        return result;
    }

    private void swap(final List<LispValue> data, final int i1, final int i2) {
        final LispValue iValue = data.get(i1);
        data.set(i1, data.get(i2));
        data.set(i2, iValue);
    }
}
