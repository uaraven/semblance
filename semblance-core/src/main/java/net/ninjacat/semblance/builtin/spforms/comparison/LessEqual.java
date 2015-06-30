package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.utils.Values;

/**
 * &lt;= evaluator
 */
public class LessEqual extends BaseComparison {

    /**
     * Creates a new instance.
     */
    public LessEqual() {
        super("<=", "&rest", "values");
    }

    @Override
    public LispValue doApply(final LispCollection parameters) {
        final LispValue value = parameters.head();

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
