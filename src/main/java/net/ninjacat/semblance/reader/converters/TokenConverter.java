package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.Token;

public interface TokenConverter {
    LispValue mkValue(Token token);
}
