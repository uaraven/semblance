package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * @author oleksiivoronin, date: 15-03-06.
 */
public final class VarBinder {

    private VarBinder() {
    }

    public static LispValue bindVariables(final Context context, final LispCollection parameters) {
        final LispValue binding = parameters.head();
        LispValue value = NilCollection.INSTANCE;
        if (isList(binding)) {
            final SList bindingList = asSList(binding);
            final SymbolAtom name = asSymbol(bindingList.head());
            value = context.evaluate(bindingList.tail().head());
            context.bind(name, value);
        } else {
            context.bind(asSymbol(binding), value);
        }
        if (parameters.tail().isNil()) {
            return value;
        } else {
            return bindVariables(context, parameters.tail());
        }
    }

}
