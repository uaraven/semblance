package net.ninjacat.semblance.builtin.spforms.comparison;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;

/**
 * Base class for all comparison operators
 */
abstract class BaseComparison extends SpecialForm {

    BaseComparison(final String... definition) {
        super(definition);
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasAtLeast(2);

        final LispCollection evaluated = context.evaluateList(parameters);

        return doApply(evaluated);
    }

    protected abstract LispValue doApply(LispCollection evaluated);
}
