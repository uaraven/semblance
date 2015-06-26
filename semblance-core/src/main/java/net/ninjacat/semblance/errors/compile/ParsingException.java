package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created on 25/02/15.
 */
public class ParsingException extends Exception {
    private final SourceInfo sourceInfo;

    /**
     * Creates new exception.
     *
     * @param message    Message.
     * @param sourceInfo Source information.
     */
    public ParsingException(final String message, final SourceInfo sourceInfo) {
        super(message);
        this.sourceInfo = sourceInfo;
    }

    /**
     * Creates new exception.
     *
     * @param message    Message.
     * @param e          Throwable that caused the exception.
     * @param sourceInfo Source information.
     */
    public ParsingException(final String message, final Throwable e, final SourceInfo sourceInfo) {
        super(message, e);
        this.sourceInfo = sourceInfo;
    }

    /**
     * @return {@link SourceInfo}
     */
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
