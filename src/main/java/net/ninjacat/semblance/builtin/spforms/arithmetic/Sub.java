package net.ninjacat.semblance.builtin.spforms.arithmetic;

import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Subtraction
 */
public class Sub extends SpecialForm {

    /**
     * Creates new instance
     */
    public Sub() {
        super("-", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection evaluated = context.evaluateList(parameters);
        NumberAtom accumulator = asNumber(evaluated.head());
        for (final LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.sub(asNumber(value));
        }
        return accumulator;
    }
}
