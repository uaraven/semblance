package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.BuiltInFunction;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Division
 */
@SuppressWarnings("ClassNamingConvention")
public class Div extends BuiltInFunction {

    private static final long serialVersionUID = -1246180267068398289L;

    /**
     * Creates new instance
     */
    public Div() {
        super("/", "&rest", "values");
    }

    @Override
    public LispValue applyFunction(final Context context, final LispCollection evaluated) {
        NumberAtom accumulator = asNumber(evaluated.head());
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.div(asNumber(value));
        }
        return accumulator;
    }
}
