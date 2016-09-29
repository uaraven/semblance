package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Base class for collection special forms
 */
public abstract class CollectionBuiltIn extends BuiltInFunction {
    private static final long serialVersionUID = -1724417384431127155L;

    protected CollectionBuiltIn(final String... definition) {
        super(definition);
    }

    protected static int findInCollection(final LispCollection lispValues, final LispValue itemToFind) {
        return lispValues.indexOf(itemToFind);
    }
}
