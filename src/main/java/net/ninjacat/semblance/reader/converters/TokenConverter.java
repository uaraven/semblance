package net.ninjacat.semblance.reader.converters;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.reader.Token;

/**
 * Interface for converting reader tokens into Semblance Values.
 */
public interface TokenConverter {
    /**
     * Coverts token into Semblance value.
     *
     * @param token Token to convert.
     * @return Semblance value.
     */
    LispValue mkValue(Token token);
}
