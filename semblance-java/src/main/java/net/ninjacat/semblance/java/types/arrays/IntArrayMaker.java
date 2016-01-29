package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a int[] array from {@link LispCollection}
 */
public class IntArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final int[] result = new int[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Integer) CallHelpers.convertValue(int.class, value);
            i += 1;
        }
        return result;
    }
}
