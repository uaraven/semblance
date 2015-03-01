package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.reader.Token;

/**
 * Created on 27/02/15.
 */
public class UnknownExpressionException extends ParsingException {
    public UnknownExpressionException(Token token) {
        super("Unknown expression " + token.getValue(), token.getSourceInfo());
    }
}
