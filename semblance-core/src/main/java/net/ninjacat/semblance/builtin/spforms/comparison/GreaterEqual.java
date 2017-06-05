package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.utils.Values;

/**
 * &gt;= evaluator
 */
public class GreaterEqual extends BaseComparison {

    /**
     * Creates a new instance.
     */
    public GreaterEqual() {
        super(">=", "&rest", "values");
    }

    @Override
    public LispValue doApply(final LispCollection parameters) {
        final LispValue value = parameters.head();

        for (final LispValue current : parameters.tail()) {
            final NumberAtom previous = Values.asNumber(value);
            final NumberAtom currentNumber = Values.asNumber(current);
            //noinspection unchecked
            if (!previous.gt(currentNumber) && !previous.eq(currentNumber)) {
                return Constants.FALSE;
            }
        }
        return Constants.TRUE;
    }
}
