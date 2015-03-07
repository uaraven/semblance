package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.list;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * (var name value [name value]...)
 */
public class Var extends SpecialForm {

    public Var() {
        super(list(symbol("var"), symbol("&rest"), symbol("bindings")));
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return VarBinder.bindVariables(context, parameters);
    }

}
