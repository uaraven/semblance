package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;

import static net.ninjacat.semblance.utils.Values.asNumber;

/**
 * Compatibility handler for Numeric types
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

    private static final Map<Class, Func<Object, LispValue>> CONVERT_MAP = Maps.of(
            Double.class, DOUBLE_FUNC,
            double.class, DOUBLE_FUNC,
            Float.class, FLOAT_FUNC,
            float.class, FLOAT_FUNC
    );

    @Override
    public boolean isCompatible(@Nonnull final Class<?> javaType, @Nullable final LispValue value) {
        return javaType.isAssignableFrom(Double.class)
                || javaType.isAssignableFrom(Float.class)
                || javaType.isAssignableFrom(double.class)
                || javaType.isAssignableFrom(float.class);
    }

    @Override
    public <T> T convertToJava(@Nonnull final Class<T> javaType, @Nonnull final LispValue value) {
        return (T) CONVERT_MAP.get(javaType).apply(value);
    }

}
