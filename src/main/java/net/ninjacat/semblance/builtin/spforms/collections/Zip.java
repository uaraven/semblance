package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * zip operation
 */
@SuppressWarnings("ClassNamingConvention")
public class Zip extends SpecialForm {

    /**
     * Creates new instance
     */
    public Zip() {
        super("list/zip", "collection1", "collection2");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);
        final LispCollection main = asCollection(evaluated.head());
        final LispCollection rest = evaluated.tail();

        final List<Iterator<LispValue>> iters = Iter.of(rest.getCollection()).map(new Func<Iterator<LispValue>, LispValue>() {
            @Override
            public Iterator<LispValue> apply(final LispValue value) {
                return asCollection(value).iterator();
            }
        }).toList();

        final List<LispValue> result = new ArrayList<>();

        for (final LispValue item : main) {
            final List<LispValue> zipped = new ArrayList<>();
            zipped.add(item);
            addAll(iters, zipped);

            result.add(new SList(zipped));
        }

        return main.createNew(result);
    }

    private void addAll(final List<Iterator<LispValue>> iters, final List<LispValue> zipped) {
        for (final Iterator<LispValue> iter : iters) {
            if (iter.hasNext()) {
                zipped.add(iter.next());
            } else {
                zipped.add(NilCollection.INSTANCE);
            }
        }
    }
}
