package net.ninjacat.semblance.java.types;

import com.google.common.collect.ImmutableMap;
import net.ninjacat.semblance.data.collections.LispValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.Map;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Compatibility handler for Integer types
 */
@SuppressWarnings("unchecked")
public class NumberTypeCompatibility implements TypeCompatibility {

    private static final NumberConverter<Integer> INT_FUNC = it -> (int) asNumber(it).longValue();
    private static final NumberConverter<Long> LONG_FUNC = it -> asNumber(it).longValue();
    private static final NumberConverter<Byte> BYTE_FUNC = it -> (byte) asNumber(it).longValue();
    private static final NumberConverter<Short> SHORT_FUNC = it -> (short) asNumber(it).longValue();
    private static final NumberConverter<Character> CHAR_FUNC = it -> (char) asNumber(it).longValue();
    private static final NumberConverter<BigInteger> BIGINT_FUNC = it -> asNumber(it).bigIntValue();
    private static final NumberConverter<Float> FLOAT_FUNC = it -> (float) asNumber(it).doubleValue();
    private static final NumberConverter<Double> DOUBLE_FUNC = it -> asNumber(it).doubleValue();
    private static final Map<Type, NumberConverter<?>> CONVERT_MAP = ImmutableMap.<Type, NumberConverter<?>>builder()
            .put(Integer.class, INT_FUNC)
            .put(int.class, INT_FUNC)
            .put(Long.class, LONG_FUNC)
            .put(long.class, LONG_FUNC)
            .put(Byte.class, BYTE_FUNC)
            .put(byte.class, BYTE_FUNC)
            .put(Short.class, SHORT_FUNC)
            .put(short.class, SHORT_FUNC)
            .put(Character.class, CHAR_FUNC)
            .put(char.class, CHAR_FUNC)
            .put(BigInteger.class, BIGINT_FUNC)
            .put(float.class, FLOAT_FUNC)
            .put(Float.class, FLOAT_FUNC)
            .put(double.class, DOUBLE_FUNC)
            .put(Double.class, DOUBLE_FUNC)
            .put(Object.class, LONG_FUNC)
            .build();

    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        if (!(javaType instanceof Class)) {
            return false;
        }
        final Class clazz = (Class) javaType;
        return clazz.isAssignableFrom(Integer.class)
                || clazz.isAssignableFrom(Long.class)
                || clazz.isAssignableFrom(Byte.class)
                || clazz.isAssignableFrom(Short.class)
                || clazz.isAssignableFrom(Character.class)
                || clazz.isAssignableFrom(BigInteger.class)
                || clazz.isAssignableFrom(Float.class)
                || clazz.isAssignableFrom(Double.class)
                || clazz.isAssignableFrom(int.class)
                || clazz.isAssignableFrom(long.class)
                || clazz.isAssignableFrom(byte.class)
                || clazz.isAssignableFrom(short.class)
                || clazz.isAssignableFrom(char.class)
                || clazz.isAssignableFrom(float.class)
                || clazz.isAssignableFrom(double.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        return (T) CONVERT_MAP.get(javaType).convert(value);
    }

    @FunctionalInterface
    private interface NumberConverter<T> {
        T convert(LispValue item);
    }

}
