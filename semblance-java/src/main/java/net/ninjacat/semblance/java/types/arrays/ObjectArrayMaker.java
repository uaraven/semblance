package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;
import java.lang.reflect.Array;
import java.util.ArrayList;

/**
 * Creates a object[] array from {@link LispCollection}
 */
public class ObjectArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final Object[] result = new Object[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = ((JavaConvertible) value).asJavaObject();
            i += 1;
        }
        return result;
    }

    @Nonnull
    @Override
    public LispCollection convertFromJavaArray(@Nonnull final Object array) {
        final ArrayList<LispValue> values = new ArrayList<>();
        for (int i = 0; i < Array.getLength(array); i++) {
            values.add(CallHelpers.toLispValue(Array.get(array, i)));
        }
        return new SList(values);
    }
}
