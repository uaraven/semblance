package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

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
    boolean isCompatible(@Nonnull Class<?> javaType, @Nullable final LispValue value);

    /**
     * @param javaType
     * @param value
     * @param <T>
     * @return
     */
    <T> T convertToJava(@Nonnull Class<T> javaType, @Nonnull LispValue value);
}
