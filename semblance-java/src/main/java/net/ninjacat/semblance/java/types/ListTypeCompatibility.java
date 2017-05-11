package net.ninjacat.semblance.java.types;

import com.google.common.collect.ImmutableSet;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.semblance.java.JavaInteropException;
import net.ninjacat.semblance.utils.Maps;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Compatibility handler for collections
 */
public class ListTypeCompatibility implements TypeCompatibility {
    private static final Map<Class, Class> PRIMITIVE_MAP = Maps.of(
            int[].class, int.class,
            long[].class, long.class,
            byte[].class, byte.class,
            short[].class, short.class,
            char[].class, char.class,
            double[].class, double.class,
            float[].class, float.class,
            String[].class, String.class
    );

    private static final Function<LispValue, Object> SIMPLE_VALUE_CONVERTER = item -> {
        if (item instanceof JavaConvertible) {
            return ((JavaConvertible) item).asJavaObject();
        } else {
            throw new JavaInteropException("Cannot convert " + item + " into Java object", SourceInfo.UNKNOWN);
        }
    };

    @Override
    public boolean isCompatible(@Nonnull final Type javaType, @Nullable final LispValue value) {
        if (javaType instanceof Class) {
            return isClassCompatible((Class) javaType);
        } else if (javaType instanceof ParameterizedType) {
            return isParameterizedTypeCompatible((ParameterizedType) javaType);
        } else {
            return false;
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T convertToJava(@Nonnull final Type javaType, @Nonnull final LispValue value) {
        final LispCollection collection = Values.asCollection(value);
        if (javaType instanceof Class) {
            return convertToJavaClass((Class<?>) javaType, collection);
        } else if (javaType instanceof ParameterizedType) {
            return (T) convertToParameterizedJavaClass((ParameterizedType) javaType, collection);
        } else {
            throw new JavaInteropException(String.format("Cannot convert %s into %s", value, javaType), SourceInfo.UNKNOWN);
        }
    }

    private static boolean isParameterizedTypeCompatible(final ParameterizedType javaType) {
        return isClassCompatible((Class) javaType.getRawType());
    }

    private static boolean isClassCompatible(@Nonnull final Class<?> javaType) {
        return javaType.isArray() ||
                Arrays.stream(javaType.getInterfaces())
                        .anyMatch(it -> matchesClass(it, Collection.class, Iterable.class));
    }

    private static List<?> convertToParameterizedJavaClass(final ParameterizedType javaType,
                                                           final LispCollection collection) {
        if (javaType.getActualTypeArguments().length != 1) {
            return convertToJavaClass((Class) javaType.getRawType(), collection);
        }
        final Class<?> genericClass = (Class) javaType.getActualTypeArguments()[0];
        return collection.stream()
                .map(value -> CallHelpers.convertValue(genericClass, value)).collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    private static <T> T convertToJavaClass(@Nonnull final Class<?> javaType,
                                            final LispCollection collection) {
        if (javaType.isArray()) {
            return (T) convertToArray(javaType, collection);
        } else {
            return (T) collection.stream().map(SIMPLE_VALUE_CONVERTER).collect(Collectors.toList());
        }
    }

    private static Object convertToArray(final Class<?> javaType, final LispCollection values) {
        if (PRIMITIVE_MAP.containsKey(javaType)) {
            return ArrayHelpers.convertToArray(PRIMITIVE_MAP.get(javaType), values);
        }
        final Class elementType = CallHelpers.getArrayElementType(javaType);
        return ArrayHelpers.convertToArray(elementType, values);
    }

    private static boolean matchesClass(final Class<?> aClass, final Class<?>... matches) {
        return ImmutableSet.copyOf(matches).contains(aClass);
    }

}
