package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a long[] array from {@link LispCollection}
 */
public class LongArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final long[] result = new long[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Long) CallHelpers.convertValue(long.class, value);
            i += 1;
        }
        return result;
    }
}
