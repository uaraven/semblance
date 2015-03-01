package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 01/03/15.
 */
public class ParameterValueExpectedException extends ParameterException {
    public ParameterValueExpectedException(SymbolAtom name, SourceInfo sourceInfo) {
        super(String.format("Parameter %s expected", name.toString()), sourceInfo);
    }
}
