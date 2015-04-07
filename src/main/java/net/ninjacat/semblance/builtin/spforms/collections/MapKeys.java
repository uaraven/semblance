package net.ninjacat.semblance.builtin.spforms.collections;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSMap;

/**
 * Keys special form
 */
public class MapKeys extends SpecialForm {
    /**
     * Create a new instance.
     */
    public MapKeys() {
        super("keys", "map-form");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return asSMap(context.evaluate(parameters.head())).keys();
    }
}
