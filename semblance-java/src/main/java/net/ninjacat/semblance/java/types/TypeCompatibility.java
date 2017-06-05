package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.LispValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;

/**
 * Defines a way to check compatibility between Semblance and Java types
 */
public interface TypeCompatibility {
    /**
     * Verifies if this semblance type is compatible with Java type
     *
     * @param javaType Java type
     * @param value    Semblance value
     * @return {@code true} or {@code false}
     */
    boolean isCompatible(@Nonnull Type javaType, @Nullable final LispValue value);

    /**
     * Converts Semblance value into Java object of correct type
     *
     * @param javaType Required java type
     * @param value    Semblance value.
     * @param <T>      Resulting type
     * @return Java object.
     */
    <T> T convertToJava(@Nonnull Type javaType, @Nonnull LispValue value);
}
