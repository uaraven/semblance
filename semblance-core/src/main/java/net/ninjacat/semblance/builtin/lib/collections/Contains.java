package net.ninjacat.semblance.builtin.lib.collections;

import net.ninjacat.semblance.builtin.spforms.collections.CollectionBuiltIn;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Checks whether collection contains an element
 */
public class Contains extends CollectionBuiltIn {

    private static final long serialVersionUID = 4944553991648675782L;

    /**
     * Creates a new instance.
     */
    public Contains() {
        super("contains", "list-form", "value");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue collection = evaluated.head();
        final LispValue itemToFind = evaluated.tail().head();

        if (isMap(collection)) {
            return booleanAsAtom(isExistInMap(asSMap(collection), itemToFind));
        } else {
            return booleanAsAtom(findInCollection(asCollection(collection), itemToFind) >= 0);
        }
    }

    private static boolean isExistInMap(final SMap sMap, final LispValue itemToFind) {
        return sMap.contains(itemToFind);
    }
}
