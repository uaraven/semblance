package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.evaluator.Context;

/**
 * Created on 28/02/15.
 */
public class PositionalParameter extends BaseParameter {

    /**
     * Create new instance of formal parameter.
     *
     * @param name Parameter name.
     */
    public PositionalParameter(final SymbolAtom name) {
        super(name);
    }

    @Override
    public boolean isRequired() {
        return true;
    }

    /**
     * Binds value to a parameter name in a given context.
     *
     * @param context     {@link Context}
     * @param actualValue Actual parameter value.
     */
    @Override
    public void bindInContext(final Context context, final LispValue actualValue) {
        context.bind(getName(), actualValue);
    }

}
