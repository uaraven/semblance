package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.DoubleNumberAtom;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.reader.Token;

/**
 * Created on 27/02/15.
 */
public class DoubleConverter implements TokenConverter {
    @Override
    public LispValue mkValue(Token token) {
        return new DoubleNumberAtom(Double.parseDouble(token.getValue()), token.getSourceInfo());
    }
}
