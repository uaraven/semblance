package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;

/**
 * Implementation of quote special form
 */
public class Quote extends SpecialForm {

    /**
     * Creates a new instance of quote special form
     */
    public Quote() {
        super("quote", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasExactly(1);
        return parameters.head();
    }
}
