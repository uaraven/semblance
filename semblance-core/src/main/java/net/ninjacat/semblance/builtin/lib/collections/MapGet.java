package net.ninjacat.semblance.builtin.lib.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SMap;
import net.ninjacat.semblance.errors.runtime.NotEnoughParametersException;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asSMap;

/**
 * Keys special form
 */
public class MapGet extends SpecialForm {
    private static final long serialVersionUID = 8727015614528586555L;

    /**
     * Create a new instance.
     */
    public MapGet() {
        super("map/get", "map-form", "map-key", "default-value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (parameters.length() < 2) {
            throw new NotEnoughParametersException("map/get expects 'key' ['defaultValue']", parameters.getSourceInfo());
        }
        final SMap map = asSMap(context.evaluate(parameters.head()));
        final LispValue key = context.evaluate(parameters.get(1));
        if (parameters.length() == 3) {
            final LispValue defaultValue = context.evaluate(parameters.get(2));
            return map.getDefault(key, defaultValue);
        } else {
            return map.get(key);
        }
    }
}
