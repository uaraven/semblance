package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaInteropException;
import net.ninjacat.semblance.java.JavaTypeConversionException;
import net.ninjacat.semblance.java.JavaWrapperValue;
import net.ninjacat.semblance.java.Symbol;
import net.ninjacat.semblance.utils.Maps;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * Utility class with helpers to convert between Semblance and Java types for parameters
 */
public final class CallHelpers {

    private static final Map<SemblanceType, ? extends TypeCompatibility> TYPE_MAP = Maps.of(
            SemblanceType.INTEGER, new NumberTypeCompatibility(),
            SemblanceType.FLOATIG_POINT, new NumberTypeCompatibility(),
            SemblanceType.STRING, new StringTypeCompatibility(),
            SemblanceType.OPAQUE, new OpaqueTypeCompatibility(),
            SemblanceType.LIST, new ListTypeCompatibility(),
            SemblanceType.VECTOR, new ListTypeCompatibility(),
            SemblanceType.SYMBOL, new SymbolCompatibility()
    );

    private CallHelpers() {
    }

    /**
     * Looks up constructor matching list of Semblance values. Will take all possible conversions into account.
     *
     * @param constructors List of constructors.
     * @param params       List of values used as parameter for constructor
     * @return Optional of found constructor
     */
    public static Optional<Constructor> findMatchingConstructor(final Constructor[] constructors,
                                                                final LispCollection params) {
        final LispValue[] values = convertToArray(params);
        return Arrays.stream(constructors)
                .filter(it -> isCompatible(values, it.getGenericParameterTypes()))
                .findFirst();
    }

    /**
     * Looks up methods matching list of Semblance values.
     *
     * @param methods List of methods
     * @param params  List of values used as parameters
     * @return Optional of method
     */
    public static Optional<Method> findMatchingMethod(final Collection<Method> methods, final LispCollection params) {
        final LispValue[] values = convertToArray(params);
        return methods.stream().filter(it -> isCompatible(values, it.getGenericParameterTypes()))
                .findFirst();
    }

    /**
     * Converts list of Semblance parameters into array of objects of required Java types.
     *
     * @param requiredTypes Array of required Java types
     * @param params        List of parameters
     * @return Array of Java objects coverted from Semblance values.
     */
    public static Object[] convertParameters(final Type[] requiredTypes,
                                             final LispCollection params) {
        if (requiredTypes.length != params.length()) {
            throw new IllegalArgumentException("Invalid number of parameters");
        }
        final Object[] results = new Object[requiredTypes.length];

        for (int i = 0; i < results.length; i++) {
            final Type requiredType = requiredTypes[i];
            final LispValue param = params.get(i);

            results[i] = convertValue(requiredType, param);
        }

        return results;
    }

    /**
     * Converts a single Semblance value into a Java object of required type
     *
     * @param requiredType Required Java type
     * @param param        Semblance Value
     * @return Resulting Java object.
     */
    public static Object convertValue(final Type requiredType, final LispValue param) {
        if (!TYPE_MAP.containsKey(param.getType())) {
            throw new JavaTypeConversionException(param, requiredType);
        }
        return TYPE_MAP.get(param.getType()).convertToJava(requiredType, param);
    }

    /**
     * Converts Java object into best matching Semblance Value
     *
     * @param pojo Java Object
     * @return Value
     */
    public static LispValue toLispValue(final Object pojo) {
        if (pojo instanceof Number) {
            return convertNumber(pojo);
        } else if (pojo instanceof String) {
            return string((String) pojo);
        } else if (pojo instanceof Symbol) {
            return symbol(((Symbol) pojo).getValue());
        } else if (pojo instanceof Boolean) {
            return (Boolean) pojo ? Constants.TRUE : Constants.FALSE;
        } else {
            if (pojo instanceof Iterable) {
                return toLispCollection((Iterable<? extends Object>) pojo);
            } else if (pojo.getClass().isArray()) {
                return ArrayHelpers.convertFromArray(pojo);
            } else {
                return new JavaWrapperValue(pojo);
            }
        }
    }

    /**
     * Extracts element class from array class
     *
     * @param javaType Array class
     * @return Class of array's element
     */
    public static Class getArrayElementType(final Class javaType) {
        final String elementTypeName = javaType.getCanonicalName().substring(0, javaType.getCanonicalName().length() - 2);
        try {
            return Class.forName(elementTypeName);
        } catch (final ClassNotFoundException e) {
            throw new JavaInteropException("Cannot determine array element type for " + javaType.getCanonicalName(), SourceInfo.UNKNOWN, e);
        }
    }

    private static LispValue convertNumber(final Object pojo) {
        if (pojo instanceof Float || pojo instanceof Double) {
            return doubleN(((Number) pojo).doubleValue());
        } else if (pojo instanceof BigInteger) {
            return bigN((BigInteger) pojo);
        } else {
            return longN(((Number) pojo).longValue());
        }
    }

    private static LispValue toLispCollection(final Iterable<? extends Object> pojo) {
        return new SList(StreamSupport.stream(pojo.spliterator(), true)
                .map(CallHelpers::toLispValue).collect(Collectors.toList()));
    }

    private static LispValue[] convertToArray(final LispCollection params) {
        return params.stream().toArray(LispValue[]::new);
    }

    private static boolean isCompatible(final LispValue[] values, final Type[] javaTypes) {
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

    private static boolean isCompatible(final LispValue value, final Type javaType) {
        return TYPE_MAP.containsKey(value.getType()) && TYPE_MAP.get(value.getType()).isCompatible(javaType, value);
    }

}
