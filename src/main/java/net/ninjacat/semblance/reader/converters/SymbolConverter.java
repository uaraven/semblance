package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.Token;

/**
 * Created on 27/02/15.
 */
public class SymbolConverter implements TokenConverter {
    @Override
    public LispValue mkValue(final Token token) {
        return new SymbolAtom(token.getValue(), token.getSourceInfo());
    }
}
