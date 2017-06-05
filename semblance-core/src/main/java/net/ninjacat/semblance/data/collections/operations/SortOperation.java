package net.ninjacat.semblance.data.collections.operations;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Sorts collection
 */
public class SortOperation implements ListOperation {

    private static final SymbolAtom DESC = new SymbolAtom(":desc");

    @Override
    public LispValue apply(@Nonnull final Context context,
                           @Nonnull final LispCollection source,
                           @Nonnull final LispCollection parameters) {
        final int direction = (!parameters.isNil() && parameters.head().equals(DESC)) ? -1 : 1;
        final List<LispValue> data = new ArrayList<>(source.getCollection());
        Collections.sort(data, new Comparator<LispValue>() {
            @Override
            public int compare(final LispValue o1, final LispValue o2) {
                return o1.compareTo(o2) * direction;
            }
        });
        return source.createNew(data);
    }
}
