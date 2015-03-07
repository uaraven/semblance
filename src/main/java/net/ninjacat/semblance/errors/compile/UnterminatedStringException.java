package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created: raven
 * Date: 25/02/15.
 */
public class UnterminatedStringException extends ParsingException {

    /**
     * Creates new exception
     *
     * @param sourceInfo Source code information.
     */
    public UnterminatedStringException(final SourceInfo sourceInfo) {
        super("Unterminated string", sourceInfo);
    }
}
