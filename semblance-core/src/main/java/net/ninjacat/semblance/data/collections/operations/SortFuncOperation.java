package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.BoundLispCallable;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Sorts collection
 */
public class SortFuncOperation implements ListOperation {

    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final BoundLispCallable sorter = new BoundLispCallable(context, asCallable(parameters.head()));
        final List<LispValue> data = new ArrayList<>(source.getCollection());
        Collections.sort(data, new Comparator<LispValue>() {
            @Override
            public int compare(final LispValue o1, final LispValue o2) {
                return (int) asNumber(sorter.apply(list(o1, o2))).longValue();
            }
        });
        return source.createNew(data);
    }
}
