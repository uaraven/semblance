package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;

import java.lang.reflect.Type;

/**
 * Semblance to Java type conversion exception.
 */
public class JavaTypeConversionException extends JavaInteropException {

    /**
     * Creates a new instance of JavaTypeConversionException.
     *
     * @param value      Semblance value to be converted.
     * @param targetType Target Java type.
     */
    public JavaTypeConversionException(final LispValue value, final Type targetType) {
        super(getMessage(value, targetType), SourceInfo.UNKNOWN);
    }

    /**
     * Creates a new instance of JavaTypeConversionException.
     *
     * @param value      Semblance value to be converted.
     * @param targetType Target Java type.
     * @param sourceInfo Source code information.
     */
    public JavaTypeConversionException(final LispValue value, final Type targetType, final SourceInfo sourceInfo) {
        super(getMessage(value, targetType), SourceInfo.UNKNOWN);
    }

    private static String getMessage(final LispValue value, final Type targetType) {
        return String.format("Cannot convert %s into %s", value, targetType);
    }
}
