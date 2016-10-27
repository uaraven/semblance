package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.special.RecursiveCallValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Recursive call into self.
 */
public class Recur extends SpecialForm {

    private static final long serialVersionUID = 6259546687296239467L;

    /**
     * Create new instance.
     */
    public Recur() {
        super("recur", "&rest", "parameters");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection params = context.evaluateList(parameters);
        return new RecursiveCallValue(params);
    }
}
