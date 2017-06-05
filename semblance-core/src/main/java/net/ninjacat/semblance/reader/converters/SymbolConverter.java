package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.reader.Token;

/**
 * Converts symbol token to {@link SymbolAtom}
 */
public class SymbolConverter implements TokenConverter {
    @Override
    public LispValue mkValue(final Token token) {
        return new SymbolAtom(token.getValue(), token.getSourceInfo());
    }
}
