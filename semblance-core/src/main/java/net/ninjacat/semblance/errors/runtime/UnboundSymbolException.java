package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 24/02/15.
 */
public class UnboundSymbolException extends SemblanceRuntimeException {

    /**
     * Creates new exception
     *
     * @param symbol     Symbol.
     * @param sourceInfo Source code information.
     */
    public UnboundSymbolException(final SymbolAtom symbol, final SourceInfo sourceInfo) {
        super(String.format("Symbol %s is unbound", symbol), sourceInfo);
    }
}
