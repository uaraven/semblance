package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Basic class for runtime errors
 */
public class SemblanceRuntimeException extends RuntimeException {

    private final String message;
    private final SourceInfo sourceInfo;

    public SemblanceRuntimeException(String message, SourceInfo sourceInfo) {
        super(message + " at " + sourceInfo);
        this.message = message;
        this.sourceInfo = sourceInfo;
    }

    public SemblanceRuntimeException(String message, SourceInfo sourceInfo, Throwable cause) {
        super(message + " at " + sourceInfo, cause);
        this.message = message;
        this.sourceInfo = sourceInfo;
    }

    @Override
    public String getMessage() {
        return message;
    }

    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
