package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 01/03/15.
 */
public class ParameterValueExpectedException extends ParameterException {
    private static final long serialVersionUID = -9023201964171030089L;

    /**
     * Creates new exception.
     *
     * @param name       Name of the expected parameter.
     * @param sourceInfo Source information.
     */
    public ParameterValueExpectedException(final SymbolAtom name, final SourceInfo sourceInfo) {
        super(String.format("Parameter %s expected", name.toString()), sourceInfo);
    }
}
