package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;
import java.util.ArrayList;

/**
 * Converts String arrays
 */
public class StringArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final String[] result = new String[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (String) CallHelpers.convertValue(String.class, value);
            i += 1;
        }
        return result;
    }

    @Nonnull
    @Override
    public LispCollection convertFromJavaArray(@Nonnull final Object array) {
        final String[] data = (String[]) array;
        final ArrayList<LispValue> values = new ArrayList<>();
        for (final String item : data) {
            values.add(CallHelpers.toLispValue(item));
        }
        return new SList(values);
    }
}
