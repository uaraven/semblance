package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingRuntimeException;

/**
 * Created on 27/02/15.
 */
public class UnexpectedEndRuntimeException extends ParsingRuntimeException {
    public UnexpectedEndRuntimeException() {
        super("Unexpected end of input", SourceInfo.UNKNOWN);
    }
}
