package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Require;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public class Quote extends SpecialForm {

    public Quote() {
        super("quote", "value");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        Require.that(parameters).hasExactly(1);
        return parameters.head();
    }
}
