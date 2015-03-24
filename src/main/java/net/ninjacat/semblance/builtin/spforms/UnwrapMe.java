package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

/**
 * This is implementation of comma operator which will just evaluate its only parameter.
 */
public class UnwrapMe extends SpecialForm {

    /**
     * Creates new instance of comma operator special form.
     */
    public UnwrapMe() {
        super(Constants.HiddenFunctions.UNWRAP.repr(), "expression");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return context.evaluate(parameters.head());
    }
}
