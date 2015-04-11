package net.ninjacat.semblance;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Update already declared binding or create new in current context
 */
public class Update extends SpecialForm {

    /**
     * Creates new instance
     */
    public Update() {
        super("set*", "var-decl*");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        return updateVariables(context, parameters);
    }

    private LispValue updateVariables(final Context context, final LispCollection parameters) {
        final LispValue binding = parameters.head();
        LispValue value = NilCollection.INSTANCE;
        if (isList(binding)) {
            final SList bindingList = asSList(binding);
            final SymbolAtom name = asSymbol(bindingList.head());
            value = context.evaluate(bindingList.tail().head());
            context.update(name, value);
        } else {
            context.update(asSymbol(binding), value);
        }
        if (parameters.tail().isNil()) {
            return value;
        } else {
            return updateVariables(context, parameters.tail());
        }
    }
}
