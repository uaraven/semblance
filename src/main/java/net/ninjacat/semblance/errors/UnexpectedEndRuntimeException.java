package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 27/02/15.
 */
public class UnexpectedEndRuntimeException extends ParsingRuntimeException {
    public UnexpectedEndRuntimeException() {
        super("Unexpected end of input", SourceInfo.UNKNOWN);
    }
}
