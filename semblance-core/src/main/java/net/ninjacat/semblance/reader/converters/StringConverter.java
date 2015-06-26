package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.StringAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.Token;

/**
 * Converts string token to {@link StringAtom}
 */
public class StringConverter implements TokenConverter {
    @Override
    public LispValue mkValue(final Token token) {
        return new StringAtom(token.getValue(), token.getSourceInfo());
    }
}
