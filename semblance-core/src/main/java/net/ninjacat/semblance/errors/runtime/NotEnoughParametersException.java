package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

/**
 * This exception is thrown when number of actual parameters to the function is less than number of expected
 * parameters
 */
public class NotEnoughParametersException extends ParameterException {
    /**
     * Creates new exception.
     *
     * @param message    Message.
     * @param sourceInfo Source information.
     */
    public NotEnoughParametersException(final String message, final SourceInfo sourceInfo) {
        super("Not enough parameters: " + message, sourceInfo);
    }
}
