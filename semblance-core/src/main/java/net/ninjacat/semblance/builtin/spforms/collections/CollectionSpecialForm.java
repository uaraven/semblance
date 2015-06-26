package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;

/**
 * Base class for collection special forms
 */
public abstract class CollectionSpecialForm extends SpecialForm {
    protected CollectionSpecialForm(final String... definition) {
        super(definition);
    }

    protected int findInCollection(final LispCollection lispValues, final LispValue itemToFind) {
        return lispValues.indexOf(itemToFind);
    }
}
