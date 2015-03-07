package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.list;
import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * @author oleksiivoronin, date: 15-03-06.
 */
public class Progn extends SpecialForm {

    /**
     * Creates instance of Progn
     */
    public Progn() {
        super(list(symbol("progn"), symbol("&rest"), symbol("expressions")));
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namelessChildContext(context);
        return localContext.evaluateBlock(parameters);
    }
}
