package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.BreakValue;
import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.SemblanceRuntimeException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;

/**
 * Loop break. Can only be called inside loop.
 */
public class Break extends SpecialForm {

    /**
     * Create new instance
     */
    public Break() {
        super("break", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (context.hasParent(Constants.LOOP)) {
            Require.that(parameters).hasAtLeast(1);
            return new BreakValue(context.evaluate(parameters.head()));
        } else {
            throw new SemblanceRuntimeException("break can only be used inside loop", parameters.getSourceInfo());
        }
    }
}
