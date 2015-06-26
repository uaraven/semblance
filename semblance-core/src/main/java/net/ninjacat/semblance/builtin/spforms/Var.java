package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * (set (name value) [(name value)]...)
 */

@SuppressWarnings("ClassNamingConvention")
public class Var extends SpecialForm {

    /**
     * Create new instance of Var special form object
     */
    public Var() {
        super("set", "&rest", "bindings");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return VarBinder.bindVariables(context, parameters);
    }

}
