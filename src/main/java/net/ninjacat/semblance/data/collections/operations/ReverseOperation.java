package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

/**
 * Reverses collection
 */
public class ReverseOperation implements ListOperation {

    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final List<LispValue> data = new ArrayList<>(source.getCollection());
        for (int i = 0; i < data.size() / 2; i++) {
            swap(data, i, data.size() - i - 1);
        }
        return source.createNew(data);
    }

    private void swap(final List<LispValue> data, final int i1, final int i2) {
        final LispValue iValue = data.get(i1);
        data.set(i1, data.get(i2));
        data.set(i2, iValue);
    }
}
