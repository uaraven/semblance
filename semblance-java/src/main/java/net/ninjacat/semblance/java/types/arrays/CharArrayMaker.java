package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a char[] array from {@link LispCollection}
 */
public class CharArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final char[] result = new char[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Character) CallHelpers.convertValue(char.class, value);
            i += 1;
        }
        return result;
    }
}
