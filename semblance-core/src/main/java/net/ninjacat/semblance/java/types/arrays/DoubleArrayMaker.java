package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;

/**
 * Creates a double[] array from {@link LispCollection}
 */
public class DoubleArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final double[] result = new double[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Double) CallHelpers.convertValue(double.class, value);
            i += 1;
        }
        return result;
    }
}
