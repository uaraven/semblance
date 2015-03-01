package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Created on 28/02/15.
 */
public class RestParameter extends BaseParameter {
    public RestParameter(SymbolAtom name) {
        super(name);
    }

    @Override
    public boolean isRequired() {
        return false;
    }

    @Override
    public void setInContext(Context context, LispValue actualValue) {
        context.bind(getName(), actualValue);
    }
}
