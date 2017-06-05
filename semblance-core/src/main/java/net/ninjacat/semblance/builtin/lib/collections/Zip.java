package net.ninjacat.semblance.builtin.lib.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * zip operation
 */
@SuppressWarnings("ClassNamingConvention")
public class Zip extends BuiltInFunction {

    private static final long serialVersionUID = 7950490641077541932L;

    /**
     * Creates new instance
     */
    public Zip() {
        super("list/zip", "collection1", "collection2");
    }

    private static void addAll(final List<Iterator<LispValue>> iters, final List<LispValue> zipped) {
        for (final Iterator<LispValue> iter : iters) {
            if (iter.hasNext()) {
                zipped.add(iter.next());
            } else {
                zipped.add(NilCollection.INSTANCE);
            }
        }
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispCollection main = asCollection(evaluated.head());
        final LispCollection rest = evaluated.tail();

        final List<Iterator<LispValue>> iters = rest.stream()
                .map(value -> asCollection(value).iterator())
                .collect(Collectors.toList());

        final List<LispValue> result = new ArrayList<>();

        for (final LispValue item : main) {
            final List<LispValue> zipped = new ArrayList<>();
            zipped.add(item);
            addAll(iters, zipped);

            result.add(new SList(zipped));
        }

        return main.createNew(result);
    }
}
