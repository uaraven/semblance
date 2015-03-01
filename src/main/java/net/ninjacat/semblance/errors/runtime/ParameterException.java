package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 01/03/15.
 */
public class ParameterException extends SemblanceRuntimeException {
    public ParameterException(String message, SourceInfo sourceInfo) {
        super(message, sourceInfo);
    }
}
