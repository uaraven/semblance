package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;
import java.util.ArrayList;

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

    @Nonnull
    @Override
    public LispCollection convertFromJavaArray(@Nonnull final Object array) {
        final float[] data = (float[]) array;
        final ArrayList<LispValue> values = new ArrayList<>();
        for (final float item : data) {
            values.add(CallHelpers.toLispValue(item));
        }
        return new SList(values);
    }
}
