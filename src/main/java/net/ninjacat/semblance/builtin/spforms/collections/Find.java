package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.number;

/**
 * Searches for an element in the collection.
 */
public class Find extends CollectionSpecialForm {

    /**
     * Create new instance.
     */
    public Find() {
        super("find", "list-form", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);

        final LispCollection collection = asCollection(evaluated.head());
        final LispValue itemToFind = evaluated.tail().head();

        return number(findInCollection(asCollection(collection), itemToFind));
    }

}
