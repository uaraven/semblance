package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 25/02/15.
 */
public class ParsingRuntimeException extends RuntimeException {
    private final SourceInfo sourceInfo;

    public ParsingRuntimeException(String message, SourceInfo sourceInfo) {
        super(message);
        this.sourceInfo = sourceInfo;
    }

    public ParsingRuntimeException(String message, Throwable e, SourceInfo sourceInfo) {
        super(message, e);
        this.sourceInfo = sourceInfo;
    }

    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
