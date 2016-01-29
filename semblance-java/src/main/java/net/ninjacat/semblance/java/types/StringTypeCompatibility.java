package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;

import static net.ninjacat.semblance.utils.Values.asString;

/**
 * Compatibility handler for Strings
 */
@SuppressWarnings("unchecked")
public class StringTypeCompatibility implements TypeCompatibility {

    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        return javaType instanceof Class && javaType.equals(String.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        return (T) asString(value).asJavaObject();
    }

}
