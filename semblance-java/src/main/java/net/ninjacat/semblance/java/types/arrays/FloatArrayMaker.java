package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a float[] array from {@link LispCollection}
 */
public class FloatArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final float[] result = new float[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Float) CallHelpers.convertValue(float.class, value);
            i += 1;
        }
        return result;
    }
}
