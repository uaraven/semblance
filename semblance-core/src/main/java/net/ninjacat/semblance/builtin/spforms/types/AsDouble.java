package net.ninjacat.semblance.builtin.spforms.types;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Collectors;
import net.ninjacat.semblance.utils.Values;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Converts number (or collection of numbers) to double
 */
public class AsDouble extends BuiltInFunction {

    private static final long serialVersionUID = -6729792106889611524L;

    /**
     * Creates a new instance of as-double function
     */
    public AsDouble() {
        super("as-double", "value");
    }

    @Override
    protected LispValue applyFunction(final Context context, final LispCollection evaluated) {
        final LispValue param = evaluated.head();
        if (isCollection(param)) {
            final LispCollection collection = asCollection(param);
            return collection.stream()
                    .map(Values::asNumber)
                    .map(NumberAtom::doubleValue)
                    .map(Values::doubleN)
                    .collect(Collectors.toSameAs(collection));
        } else {
            return doubleN(asNumber(param).doubleValue());
        }
    }
}
