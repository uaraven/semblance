package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;
import java.util.ArrayList;

/**
 * Creates a byte[] array from {@link LispCollection}
 */
public class ByteArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final byte[] result = new byte[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (Byte) CallHelpers.convertValue(byte.class, value);
            i += 1;
        }
        return result;
    }

    @Nonnull
    @Override
    public LispCollection convertFromJavaArray(@Nonnull final Object array) {
        final byte[] data = (byte[]) array;
        final ArrayList<LispValue> values = new ArrayList<>();
        for (final byte item : data) {
            values.add(CallHelpers.toLispValue(item));
        }
        return new SList(values);
    }
}
