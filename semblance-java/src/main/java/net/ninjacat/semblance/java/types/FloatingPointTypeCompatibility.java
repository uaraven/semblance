package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Type;
import java.util.Map;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Compatibility handler for floating point types
 */
@SuppressWarnings("unchecked")
public class FloatingPointTypeCompatibility implements TypeCompatibility {

    private static final Func<Object, LispValue> DOUBLE_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return asNumber(lispValue).doubleValue();
        }
    };
    private static final Func<Object, LispValue> FLOAT_FUNC = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue lispValue) {
            return (float) asNumber(lispValue).doubleValue();
        }
    };

    private static final Map<Type, Func<Object, LispValue>> CONVERT_MAP = Maps.of(
            Double.class, DOUBLE_FUNC,
            double.class, DOUBLE_FUNC,
            Float.class, FLOAT_FUNC,
            float.class, FLOAT_FUNC,
            Object.class, DOUBLE_FUNC
    );

    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        if (!(javaType instanceof Class)) {
            return false;
        }
        final Class<?> clazz = (Class<?>) javaType;
        return clazz.isAssignableFrom(Double.class)
                || clazz.isAssignableFrom(Float.class)
                || clazz.isAssignableFrom(double.class)
                || clazz.isAssignableFrom(float.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        return (T) CONVERT_MAP.get(javaType).apply(value);
    }

}
