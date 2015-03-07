package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.LocalContext;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * @author oleksiivoronin, date: 15-03-06.
 */

// TODO: To be rewritten as macro as soon as macro support and library support are added
public class Let extends SpecialForm {
    public Let() {
        super(list(symbol("let"), symbol("defines"), symbol("expressions")));
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final Context localContext = LocalContext.namelessChildContext(context);
        final LispValue bindings = parameters.head();
        final LispValue expressions = parameters.tail();

        VarBinder.bindVariables(localContext, asSList(bindings));

        return localContext.evaluateBlock(asCollection(expressions));
    }
}
