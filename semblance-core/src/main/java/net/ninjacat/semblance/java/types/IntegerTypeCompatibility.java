package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;

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

    private static final Map<Type, Func<Object, LispValue>> CONVERT_MAP = Maps.of(
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
            BigInteger.class, BIGINT_FUNC,
            Object.class, LONG_FUNC
    );

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
                || clazz.isAssignableFrom(int.class)
                || clazz.isAssignableFrom(long.class)
                || clazz.isAssignableFrom(byte.class)
                || clazz.isAssignableFrom(short.class)
                || clazz.isAssignableFrom(char.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        return (T) CONVERT_MAP.get(javaType).apply(value);
    }

}
