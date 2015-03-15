package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.InterpretedFunction;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asCollection;

/**
 * Function declaration. Returns unnamed function.
 */
public class Fn extends SpecialForm {

    public Fn() {
        super("fn", "params", "exprs");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final LispCollection funcParams = asCollection(parameters.head());
        final LispCollection body = parameters.tail();

        return new InterpretedFunction(SymbolAtom.NONE, funcParams, body);
    }
}
