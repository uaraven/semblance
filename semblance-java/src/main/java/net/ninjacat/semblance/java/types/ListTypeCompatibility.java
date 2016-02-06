package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.semblance.java.JavaInteropException;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.collections.Collect;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    private static final Func<Object, LispValue> SIMPLE_VALUE_CONVERTER = new Func<Object, LispValue>() {
        @Override
        public Object apply(final LispValue item) {
            if (item instanceof JavaConvertible) {
                return ((JavaConvertible) item).asJavaObject();
            } else {
                throw new JavaInteropException("Cannot convert " + item + " into Java object", SourceInfo.UNKNOWN);
            }
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
                Iter.of(javaType.getInterfaces()).any(new ClassPredicate(Collection.class, Iterable.class));
    }

    private static List<?> convertToParameterizedJavaClass(final ParameterizedType javaType,
                                                           final LispCollection collection) {
        if (javaType.getActualTypeArguments().length != 1) {
            return convertToJavaClass((Class) javaType.getRawType(), collection);
        }
        final Class<?> genericClass = (Class) javaType.getActualTypeArguments()[0];
        return Iter.of(collection.getCollection())
                .map(new Func<Object, LispValue>() {
                    @Override
                    public Object apply(final LispValue value) {
                        return CallHelpers.convertValue(genericClass, value);
                    }
                }).toList();
    }

    @SuppressWarnings("unchecked")
    private static <T> T convertToJavaClass(@Nonnull final Class<?> javaType,
                                            final LispCollection collection) {
        if (javaType.isArray()) {
            return (T) convertToArray(javaType, collection);
        } else {
            return (T) Iter.of(collection.getCollection()).map(SIMPLE_VALUE_CONVERTER).toList();
        }
    }

    private static Object convertToArray(final Class<?> javaType, final LispCollection values) {
        if (PRIMITIVE_MAP.containsKey(javaType)) {
            return ArrayHelpers.convertToArray(PRIMITIVE_MAP.get(javaType), values);
        }
        return Iter.of(values.getCollection()).map(SIMPLE_VALUE_CONVERTER).toArray(new Object[values.length()]);
    }

    private static final class ClassPredicate implements Predicate<Class<?>> {

        private final Set<Class<?>> matches;

        private ClassPredicate(final Class<?>... matches) {
            this.matches = Collect.setOf(matches);
        }

        @Override
        public boolean matches(final Class<?> aClass) {
            return matches.contains(aClass);
        }
    }
}
