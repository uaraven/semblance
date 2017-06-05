package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.java.JavaInteropException;
import net.ninjacat.semblance.java.JavaTypeConversionException;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.Type;

import static net.ninjacat.semblance.utils.Values.asSymbol;
import static net.ninjacat.semblance.utils.Values.getSourceInfo;

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
                && ((Constants.TRUE.equals(value) || Constants.FALSE.equals(value))
                && (javaClass.isAssignableFrom(Boolean.class) || javaClass.isAssignableFrom(boolean.class)))
                || (javaClass.isEnum());
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        if (((Class) javaType).isEnum()) {
            final Class<?> javaClass = (Class<?>) javaType;
            try {
                final Field field = javaClass.getField(asSymbol(value).asJavaObject().getValue());
                return (T) field.get(javaClass);
            } catch (final Exception ex) {
                throw new JavaInteropException(String.format("Failed to convert %s into %s", value, javaType),
                        getSourceInfo(value), ex);
            }
        } else if (Constants.TRUE.equals(asSymbol(value))) {
            return (T) Boolean.TRUE;
        } else if (Constants.FALSE.equals(asSymbol(value))) {
            return (T) Boolean.FALSE;
        } else {
            throw new JavaTypeConversionException(value, javaType);
        }
    }
}
