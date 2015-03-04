package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Created on 03/03/15.
 */
public class Div extends SpecialForm {

    public Div() {
        super(list(symbol("/"), symbol("&rest"), symbol("values")));
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        LispCollection evaluated = context.evaluateList(parameters);
        NumberAtom accumulator = asNumber(evaluated.head());
        for (LispValue value : evaluated.tail()) {
            //noinspection unchecked
            accumulator = accumulator.div(asNumber(value));
        }
        return accumulator;
    }
}
