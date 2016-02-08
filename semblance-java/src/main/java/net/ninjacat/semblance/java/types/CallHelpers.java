package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.java.JavaTypeConversionException;
import net.ninjacat.semblance.java.JavaWrapperValue;
import net.ninjacat.semblance.java.Symbol;
import net.ninjacat.smooth.collections.Maps;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.functions.Predicate;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map;

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
    public static Option<Constructor> findMatchingConstructor(final Constructor[] constructors,
                                                              final LispCollection params) {
        final LispValue[] values = convertToArray(params);
        return Option.of(Iter.of(constructors).find(new Predicate<Constructor>() {
            @Override
            public boolean matches(final Constructor constructor) {
                return isCompatible(values, constructor.getGenericParameterTypes());
            }
        }, null));
    }

    /**
     * Looks up methods matching list of Semblance values.
     *
     * @param methods List of methods
     * @param params  List of values used as parameters
     * @return Optional of method
     */
    public static Option<Method> findMatchingMethod(final Collection<Method> methods, final LispCollection params) {
        final LispValue[] values = convertToArray(params);
        return Option.of(Iter.of(methods).find(new Predicate<Method>() {
            @Override
            public boolean matches(final Method method) {
                return isCompatible(values, method.getGenericParameterTypes());
            }
        }, null));
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
        return new SList(Iter.<Object>of(pojo.iterator())
                .map(new Func<LispValue, Object>() {
                    @Override
                    public LispValue apply(final Object obj) {
                        return toLispValue(obj);
                    }
                }).toList());
    }

    private static LispValue[] convertToArray(final LispCollection params) {
        return Iter.of(params.getCollection()).toArray(new LispValue[params.length()]);
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
