package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;

import java.io.Serializable;

/**
 * Parameter description
 */
public interface Parameter extends Serializable {
    /**
     * @return parameter name
     */
    SymbolAtom getName();

    /**
     * @return whether parameter is required or not
     */
    boolean isRequired();

    /**
     * Binds value to a parameter name.
     *
     * @param context     Context to bind in.
     * @param actualValue Value to bind to parameter name.
     */
    void bindInContext(Context context, LispValue actualValue);
}
