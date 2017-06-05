package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.OpaqueValue;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;

/**
 * Compatibility handler for Object types
 */
@SuppressWarnings("unchecked")
public class OpaqueTypeCompatibility implements TypeCompatibility {

    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        return value == null || (javaType instanceof Class
                && ((Class) javaType).isAssignableFrom(Values.asOpaque(value).getValue().getClass()));
    }

    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        return (T) ((OpaqueValue) value).getValue();
    }

}
