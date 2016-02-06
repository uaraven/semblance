package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.JavaTypeConversionException;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;

import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * Implements type compatibility for symbols, now supporting only symbols of true and false
 */
public class SymbolCompatibility implements TypeCompatibility {
    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        if (!(javaType instanceof Class)) {
            return false;
        }
        final Class<?> javaClass = (Class) javaType;
        return Values.isSymbol(value)
                && (Constants.TRUE.equals(value) || Constants.FALSE.equals(value))
                && (javaClass.isAssignableFrom(Boolean.class) || javaClass.isAssignableFrom(boolean.class));
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        if (Constants.TRUE.equals(asSymbol(value))) {
            return (T) Boolean.TRUE;
        } else if (Constants.FALSE.equals(asSymbol(value))) {
            return (T) Boolean.FALSE;
        } else {
            throw new JavaTypeConversionException(value, javaType);
        }
    }
}
