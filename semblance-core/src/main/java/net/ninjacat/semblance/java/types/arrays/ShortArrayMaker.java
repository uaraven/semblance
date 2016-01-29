package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a short[] array from {@link LispCollection}
 */
public class ShortArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final short[] result = new short[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Short) CallHelpers.convertValue(short.class, value);
            i += 1;
        }
        return result;
    }
}
