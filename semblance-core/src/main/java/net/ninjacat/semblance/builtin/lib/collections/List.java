package net.ninjacat.semblance.builtin.lib.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Special form to create list of evaluated values
 */
public class List extends SpecialForm {

    private static final long serialVersionUID = -8791189222834020669L;

    /**
     * Creates new instance of list special form
     */
    public List() {
        super("list", "&rest", "elements");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return context.evaluateList(parameters);
    }
}
