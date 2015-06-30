package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.reader.Token;

/**
 * Created on 27/02/15.
 */
public class UnknownExpressionRuntimeException extends ParsingRuntimeException {

    /**
     * Creates new exception.
     *
     * @param token Encountered token.
     */
    public UnknownExpressionRuntimeException(final Token token) {
        super("Unknown expression " + token.getValue(), token.getSourceInfo());
    }
}
