package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

/**
 * (var (name value) [(name value)]...)
 */
public class Var extends SpecialForm {

    public Var() {
        super("var", "&rest", "bindings");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return VarBinder.bindVariables(context, parameters);
    }

}
