package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Created on 28/02/15.
 */
public class StandardParameter extends BaseParameter {

    public StandardParameter(SymbolAtom name) {
        super(name);
    }

    public void setInContext(Context context, LispValue actualValue) {
        context.bind(getName(), actualValue);
    }

}
