package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Created on 28/02/15.
 */
public interface Parameter {
    SymbolAtom getName();

    void setInContext(Context context, LispValue actualValue);
}
