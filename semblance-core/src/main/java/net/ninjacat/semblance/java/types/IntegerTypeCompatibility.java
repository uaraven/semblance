package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.math.BigInteger;
import java.util.Map;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Compatibility handler for Numeric types
 */
@SuppressWarnings("unchecked")
public class IntegerTypeCompatibility implements TypeCompatibility {

    private static final Func<Object, LispValue> INT_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return (int) asNumber(lispValue).longValue();
        }
    };
    private static final Func<Object, LispValue> LONG_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return asNumber(lispValue).longValue();
        }
    };
    private static final Func<Object, LispValue> BYTE_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return (byte) asNumber(lispValue).longValue();
        }
    };
    private static final Func<Object, LispValue> SHORT_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return (short) asNumber(lispValue).longValue();
        }
    };
    private static final Func<Object, LispValue> CHAR_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return (char) asNumber(lispValue).longValue();
        }
    };
    private static final Func<Object, LispValue> BIGINT_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return asNumber(lispValue).bigIntValue();
        }
    };

    private static final Map<Class, Func<Object, LispValue>> CONVERT_MAP = Maps.of(
            Integer.class, INT_FUNC,
            int.class, INT_FUNC,
            Long.class, LONG_FUNC,
            long.class, LONG_FUNC,
            Byte.class, BYTE_FUNC,
            byte.class, BYTE_FUNC,
            Short.class, SHORT_FUNC,
            short.class, SHORT_FUNC,
            Character.class, CHAR_FUNC,
            char.class, CHAR_FUNC,
            BigInteger.class, BIGINT_FUNC
    );

    @Override
    public boolean isCompatible(@Nonnull final Class<?> javaType, @Nullable final LispValue value) {
        return javaType.isAssignableFrom(Integer.class)
                || javaType.isAssignableFrom(Long.class)
                || javaType.isAssignableFrom(Byte.class)
                || javaType.isAssignableFrom(Short.class)
                || javaType.isAssignableFrom(Character.class)
                || javaType.isAssignableFrom(BigInteger.class)
                || javaType.isAssignableFrom(int.class)
                || javaType.isAssignableFrom(long.class)
                || javaType.isAssignableFrom(byte.class)
                || javaType.isAssignableFrom(short.class)
                || javaType.isAssignableFrom(char.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Class<T> javaType, @Nonnull final LispValue value) {
        return (T) CONVERT_MAP.get(javaType).apply(value);
    }

}
