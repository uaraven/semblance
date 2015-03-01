package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;

import java.io.Serializable;

/**
 * Created on 28/02/15.
 */
public interface Parameter extends Serializable {
    SymbolAtom getName();

    boolean isRequired();

    void setInContext(Context context, LispValue actualValue);
}
