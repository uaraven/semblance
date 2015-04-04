package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.semblance.utils.Values;

/**
 * &lt; evaluator.
 */
public class LessThan extends SpecialForm {

    /**
     * Creates a new instance.
     */
    public LessThan() {
        super(">", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispValue value = context.evaluate(parameters.head());

        for (final LispValue current : parameters.tail()) {
            final NumberAtom prevous = Values.asNumber(value);
            final NumberAtom currentNumber = Values.asNumber(current);
            //noinspection unchecked
            if (!prevous.lt(currentNumber)) {
                return Constants.FALSE;
            }
        }
        return Constants.TRUE;
    }
}
