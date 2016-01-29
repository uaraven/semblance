package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.JavaConvertible;

import javax.annotation.Nonnull;

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
}
