package net.ninjacat.semblance.errors.compile;

import net.ninjacat.semblance.reader.Token;

/**
 * Created on 25/02/15.
 */
public class TermExpectedException extends ParsingException {

    /**
     * Creates new exception.
     *
     * @param term   Expected term.
     * @param actual Actual term.
     */
    public TermExpectedException(final String term, final Token actual) {
        super(String.format("%s expected but %s found.", term, actual.getValue()), actual.getSourceInfo());
    }
}
