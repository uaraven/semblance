package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 25/02/15.
 */
public class ParsingRuntimeException extends RuntimeException {
    private final SourceInfo sourceInfo;

    /**
     * Creates new Exception.
     *
     * @param message    Error message.
     * @param sourceInfo Source code information.
     */
    public ParsingRuntimeException(final String message, final SourceInfo sourceInfo) {
        super(message);
        this.sourceInfo = sourceInfo;
    }

    /**
     * Creates new exception.
     *
     * @param message    Message.
     * @param e          Original cause of the error.
     * @param sourceInfo Source code information.
     */
    public ParsingRuntimeException(final String message, final Throwable e, final SourceInfo sourceInfo) {
        super(message, e);
        this.sourceInfo = sourceInfo;
    }

    /**
     * @return Source code information.
     */
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
