package net.ninjacat.semblance.java;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.java.types.*;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;

import java.lang.reflect.Constructor;
import java.util.Map;

public final class Parameters {

    private static final Map<SemblanceType, ? extends TypeCompatibility> TYPE_MAP = Maps.of(
            SemblanceType.INTEGER, new IntegerTypeCompatibility(),
            SemblanceType.FLOATIG_POINT, new FloatingPointTypeCompatibility(),
            SemblanceType.STRING, new StringTypeCompatibility(),
            SemblanceType.OPAQUE, new OpaqueTypeCompatibility()
    );

    private Parameters() {
    }

    public static Option<Constructor> findMatchingConstructor(final Constructor[] constructors,
                                                              final LispCollection params) {
        final LispValue[] values = convertToArray(params);
        return Option.of(Iter.of(constructors).find(new Predicate<Constructor>() {
            @Override
            public boolean matches(final Constructor constructor) {
                return isCompatible(values, constructor.getParameterTypes());
            }
        }, null));
    }

    public static Object[] convertParameters(final Class<?>[] requiredTypes,
                                             final LispCollection params) {
        if (requiredTypes.length != params.length()) {
            throw new IllegalArgumentException("Invalid number of parameters");
        }
        final Object[] results = new Object[requiredTypes.length];

        for (int i = 0; i < results.length; i++) {
            final Class<?> requiredType = requiredTypes[i];
            final LispValue param = params.get(i);

            results[i] = TYPE_MAP.get(param.getType()).convertToJava(requiredType, param);
        }

        return results;
    }

    private static LispValue[] convertToArray(final LispCollection params) {
        return Iter.of(params.getCollection()).toArray(new LispValue[params.length()]);
    }

    private static boolean isCompatible(final LispValue[] values, final Class<?>[] javaTypes) {
        if (values.length != javaTypes.length) {
            return false;
        }
        for (int i = 0; i < values.length; i++) {
            if (!isCompatible(values[i], javaTypes[i])) {
                return false;
            }
        }
        return true;
    }

    private static boolean isCompatible(final LispValue value, final Class<?> javaType) {
        return TYPE_MAP.containsKey(value.getType()) && TYPE_MAP.get(value.getType()).isCompatible(javaType, value);
    }

}
