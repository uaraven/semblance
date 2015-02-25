package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 25/02/15.
 */
public class ParsingException extends Exception {
    private final SourceInfo sourceInfo;

    public ParsingException(String message, SourceInfo sourceInfo) {
        super(message);
        this.sourceInfo = sourceInfo;
    }

    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
