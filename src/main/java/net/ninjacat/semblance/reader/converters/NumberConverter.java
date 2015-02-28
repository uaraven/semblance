package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NumberAtom;
import net.ninjacat.semblance.reader.Token;

/**
 * Created on 27/02/15.
 */
public class NumberConverter implements TokenConverter {
    @Override
    public LispValue mkValue(Token token) {
        return NumberAtom.make(token.getValue(), token.getSourceInfo());
    }
}
