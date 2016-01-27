package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Compatibility handler for Numeric types
 */
@SuppressWarnings("unchecked")
public class OpaqueTypeCompatibility implements TypeCompatibility {

    @Override
    public boolean isCompatible(@Nonnull final Class<?> javaType, @Nullable final LispValue value) {
        return value == null || javaType.isAssignableFrom(Values.asOpaque(value).getValue().getClass());
    }

    @Override
    public <T> T convertToJava(@Nonnull final Class<T> javaType, @Nonnull final LispValue value) {
        return (T) ((OpaqueValue) value).getValue();
    }

}
