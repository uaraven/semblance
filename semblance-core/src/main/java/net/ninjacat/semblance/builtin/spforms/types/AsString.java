package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Collectors;
import net.ninjacat.semblance.utils.Values;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Converts data to a string.
 * <p>
 * Only first parameter is converted. If the parameter is a collection, all elements inside collection will be
 * converted to string and same kind of collection will be returned
 * <p>
 */
public class AsString extends BuiltInFunction {

    private static final long serialVersionUID = 5891714102445778798L;

    public AsString() {
        super("as-string", "value");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue param = evaluated.head();
        if (isCollection(param)) {
            final LispCollection collection = asCollection(param);
            return collection.stream()
                    .map(LispValue::printIt)
                    .map(Values::string)
                    .collect(Collectors.toSameAs(collection));
        } else {
            return string(param.printIt());
        }
    }
}
