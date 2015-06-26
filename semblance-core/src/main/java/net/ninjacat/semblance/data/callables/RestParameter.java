package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Created on 28/02/15.
 */
class RestParameter extends BaseParameter {
    RestParameter(final SymbolAtom name) {
        super(name);
    }

    @Override
    public boolean isRequired() {
        return false;
    }

    @Override
    public void bindInContext(final Context context, final LispValue actualValue) {
        context.bind(getName(), actualValue);
    }
}
