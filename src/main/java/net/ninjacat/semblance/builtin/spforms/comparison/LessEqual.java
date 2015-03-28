package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;
import net.ninjacat.semblance.utils.Values;

/**
 * &lt;= evaluator
 */
public class LessEqual extends SpecialForm {

    /**
     * Creates a new instance.
     */
    public LessEqual() {
        super("<=", "&rest", "values");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispValue value = context.evaluate(parameters.head());

        for (final LispValue current : parameters.tail()) {
            final NumberAtom previous = Values.asNumber(value);
            final NumberAtom currentNumber = Values.asNumber(current);
            //noinspection unchecked
            if (!previous.lt(currentNumber) && !previous.eq(currentNumber)) {
                return Constants.FALSE;
            }
        }
        return Constants.TRUE;
    }
}
