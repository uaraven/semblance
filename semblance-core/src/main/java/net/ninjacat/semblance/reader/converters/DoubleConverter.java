package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.DoubleNumberAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.Token;

/**
 * Converts double number token into {@link DoubleNumberAtom}
 */
public class DoubleConverter implements TokenConverter {
    @Override
    public LispValue mkValue(final Token token) {
        return new DoubleNumberAtom(Double.parseDouble(token.getValue()), token.getSourceInfo());
    }
}
