package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import static net.ninjacat.semblance.utils.Values.asString;

/**
 * Compatibility handler for Numeric types
 */
@SuppressWarnings("unchecked")
public class StringTypeCompatibility implements TypeCompatibility {

    @Override
    public boolean isCompatible(@Nonnull final Class<?> javaType, @Nullable final LispValue value) {
        return javaType.equals(String.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Class<T> javaType, @Nonnull final LispValue value) {
        return (T) asString(value).asJavaObject();
    }

}
