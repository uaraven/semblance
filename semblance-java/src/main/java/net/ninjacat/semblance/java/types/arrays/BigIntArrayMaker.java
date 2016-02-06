package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.types.CallHelpers;

import javax.annotation.Nonnull;
import java.math.BigInteger;
import java.util.ArrayList;

/**
 * Creates a BigInteger[] array from {@link LispCollection}
 */
public class BigIntArrayMaker implements JavaArrayMaker {

    @Nonnull
    @Override
    public Object covertToJavaArray(@Nonnull final LispCollection collection) {
        final BigInteger[] result = new BigInteger[collection.length()];
        int i = 0;
        for (final LispValue value : collection) {
            result[i] = (BigInteger) CallHelpers.convertValue(BigInteger.class, value);
            i += 1;
        }
        return result;
    }

    @Nonnull
    @Override
    public LispCollection convertFromJavaArray(@Nonnull final Object array) {
        final BigInteger[] data = (BigInteger[]) array;
        final ArrayList<LispValue> values = new ArrayList<>();
        for (final BigInteger item : data) {
            values.add(CallHelpers.toLispValue(item));
        }
        return new SList(values);
    }
}
