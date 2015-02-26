package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * Created: raven
 * Date: 25/02/15.
 */
public class UnterminatedStringException extends ParsingException {

    public UnterminatedStringException(SourceInfo sourceInfo) {
        super("Unterminated string", sourceInfo);
    }
}
