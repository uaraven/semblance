package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

final class VarBinder {

    private VarBinder() {
    }

    static LispValue bindVariables(final Context context, final LispCollection parameters) {
        final LispValue binding = parameters.head();
        LispValue value = NilCollection.INSTANCE;
        if (isList(binding)) {
            final SList bindingList = asSList(binding);
            final SymbolAtom name = asSymbol(bindingList.head());
            if (!bindingList.tail().isNil()) {
                value = context.evaluate(bindingList.tail().head());
            }
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
