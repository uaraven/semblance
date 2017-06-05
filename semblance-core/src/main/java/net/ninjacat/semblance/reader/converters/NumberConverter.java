package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.reader.Token;

/**
 * Converts integer number token to {@link NumberAtom}
 */
public class NumberConverter implements TokenConverter {
    @Override
    public LispValue mkValue(final Token token) {
        return NumberAtom.make(token.getValue(), token.getSourceInfo());
    }
}
