package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Checks whether collection contains an element
 */
public class Contains extends CollectionSpecialForm {

    /**
     * Creates a new instance.
     */
    public Contains() {
        super("contains", "list-form", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);

        final LispValue collection = evaluated.head();
        final LispValue itemToFind = evaluated.tail().head();

        if (isMap(collection)) {
            return booleanAsAtom(isExistInMap(asSMap(collection), itemToFind));
        } else {
            return booleanAsAtom(findInCollection(asCollection(collection), itemToFind) >= 0);
        }
    }

    private boolean isExistInMap(final SMap sMap, final LispValue itemToFind) {
        return sMap.contains(itemToFind);
    }
}
