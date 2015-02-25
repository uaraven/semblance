package net.ninjacat.semblance.errors;

import net.ninjacat.semblance.reader.Token;

/**
 * Created on 25/02/15.
 */
public class TermExpectedException extends ParsingException {

    public TermExpectedException(String term, Token actual) {
        super(String.format("%s expected but %s found.", term, actual.getValue()), actual.getSourceInfo());
    }
}
