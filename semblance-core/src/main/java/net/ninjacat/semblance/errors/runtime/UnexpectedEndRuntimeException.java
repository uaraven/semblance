package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingRuntimeException;

/**
 * Created on 27/02/15.
 */
public class UnexpectedEndRuntimeException extends ParsingRuntimeException {
    private static final long serialVersionUID = 1582582638146871591L;

    /**
     * Creates new instance.
     *
     * @param sourceInfo source information for encountered stream end
     */
    public UnexpectedEndRuntimeException(final SourceInfo sourceInfo) {
        super("Unexpected end of input", sourceInfo);
    }
}
