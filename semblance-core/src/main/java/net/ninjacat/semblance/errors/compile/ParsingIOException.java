package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Exception that is thrown when I/O error happens during parsing.
 */
public class ParsingIOException extends ParsingException {

    /**
     * Creates new instance of ParsingIOError.
     *
     * @param message Message describing the problem.
     * @param e       Original exception.
     */
    public ParsingIOException(final String message, final Throwable e) {
        super(message, e, SourceInfo.UNKNOWN);
    }
}
