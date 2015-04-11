package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * (var (name value) [(name value)]...)
 */

// This has to be renamed to 'set' and set from standard.smbl should be removed to sset (as in simple set)
public class Var extends SpecialForm {

    public Var() {
        super("var", "&rest", "bindings");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return VarBinder.bindVariables(context, parameters);
    }

}
