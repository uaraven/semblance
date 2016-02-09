package net.ninjacat.semblance.builtin.lib.collections;

import net.ninjacat.semblance.builtin.spforms.collections.CollectionBuiltIn;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.number;

/**
 * Searches for an element in the collection.
 */
@SuppressWarnings("ClassNamingConvention")
public class Find extends CollectionBuiltIn {

    /**
     * Create new instance.
     */
    public Find() {
        super("list/find", "list-form", "value");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispCollection collection = asCollection(evaluated.head());
        final LispValue itemToFind = evaluated.tail().head();

        return number(findInCollection(asCollection(collection), itemToFind));
    }

}
