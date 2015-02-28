package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class UnboundSymbolException extends SemblanceRuntimeException {

    public UnboundSymbolException(SymbolAtom symbol, SourceInfo sourceInfo) {
        super(String.format("Symbol %s is unbound", symbol), sourceInfo);
    }
}
