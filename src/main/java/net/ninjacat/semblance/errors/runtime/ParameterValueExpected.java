package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.SemblanceRuntimeException;

/**
 * Created on 01/03/15.
 */
public class ParameterValueExpected extends SemblanceRuntimeException {
    public ParameterValueExpected(SymbolAtom name, SourceInfo sourceInfo) {
        super(String.format("Parameter %s expected", name.toString()), sourceInfo);
    }
}
