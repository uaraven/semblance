package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.data.collections.*;
import net.ninjacat.semblance.data.special.ReturnValue;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.semblance.java.Symbol;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import java.math.BigInteger;
import java.util.List;

/**
 * Various utils simplifying value manipulations.
 * <p>
 * This utility functions are mostly intended for internal use in Semblance interpreter but may be handy for developers
 * who embed interpreter in their projects and need to operate {@link LispValue}s.
 */
@SuppressWarnings("ConstantNamingConvention")
public final class Values {
    /**
     * TRUE constant used in {@code if} special form and elsewhere
     */
    public static final SymbolAtom T = symbol("T");

    /**
     * FALSE constant used in {@code if} special form and elsewhere
     */
    public static final SymbolAtom F = symbol("F");

    private Values() {
    }

    /**
     * Gets a long value from {@link LispValue}
     *
     * @param atom LispValue which should be an instance of {@link LongNumberAtom}
     * @return long value
     * @throws TypeMismatchException if LispValue is not LongNumberAtom
     */
    public static long getLongValue(final LispValue atom) {
        if (atom instanceof LongNumberAtom) {
            return ((LongNumberAtom) atom).asJavaObject();
        } else {
            throw new TypeMismatchException(SemblanceType.INTEGER, atom, getSourceInfo(atom));
        }
    }

    /**
     * Retrieves debugging information from an atom
     *
     * @param atom Atom to retrieve debugging information from
     * @return {@link SourceInfo} if provided atom supports debugging information or {@link SourceInfo#UNKNOWN} otherwises
     */
    public static SourceInfo getSourceInfo(final LispValue atom) {
        if (atom instanceof DebugInfoProvider) {
            return ((DebugInfoProvider) atom).getSourceInfo();
        } else {
            return SourceInfo.UNKNOWN;
        }

    }

    /**
     * Creates a number atom from a string. String must contain value which can be passed to {@link BigInteger#BigInteger(String)}
     *
     * @param value Number value
     * @return {@link LispValue} with a number atom
     */
    public static LispValue number(final String value) {
        return NumberAtom.make(value);
    }

    /**
     * Creates a number atom from long value
     *
     * @param value Long number
     * @return {@link LispValue} with a number atom
     */
    public static LispValue number(final long value) {
        return new LongNumberAtom(value);
    }

    /**
     * Creates a number atom from double value
     *
     * @param value Double number
     * @return {@link LispValue} with a number atom
     */
    public static LispValue number(final double value) {
        return new DoubleNumberAtom(value);
    }

    /**
     * Creates a string atom from Java String
     *
     * @param value String value
     * @return {@link LispValue} with a string atom
     */
    @SuppressWarnings("QuestionableName")
    public static LispValue string(final String value) {
        return new StringAtom(value);
    }

    /**
     * Creates a symbol from a string value
     *
     * @param value Symbol
     * @return {@link SymbolAtom}
     */
    public static SymbolAtom symbol(final String value) {
        return new SymbolAtom(value);
    }

    /**
     * Creates a symbol with debuggin information
     *
     * @param value      Symbol value
     * @param sourceInfo {@link SourceInfo} for debugging purposes
     * @return {@link SymbolAtom}
     */
    public static SymbolAtom symbol(final String value, final SourceInfo sourceInfo) {
        return new SymbolAtom(value, sourceInfo);
    }

    /**
     * Smart constructor for an atom.
     * <p>
     * Supports following types as parameter:
     * <ul>
     * <li>{@link String}</li>
     * <li>{@link Long}</li>
     * <li>{@link Integer}</li>
     * <li>{@link Double}</li>
     * <li>{@link Symbol}</li>
     * </ul>
     *
     * @param value Value to convert to atom
     * @return {@link LispValue}
     */
    public static LispValue atom(final Object value) {
        if (value instanceof String) {
            return string(value.toString());
        } else if (value instanceof Long) {
            return number((Long) value);
        } else if (value instanceof Integer) {
            return number(((Integer) value).longValue());
        } else if (value instanceof Double) {
            return number((double) value);
        } else if (value instanceof Symbol) {
            return symbol(((Symbol) value).getValue());
        } else {
            throw new IllegalArgumentException(String.format("Cannot convert %s to atom", value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into StringAtom, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link StringAtom}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link StringAtom}
     */
    public static StringAtom asString(final LispValue value) {
        if (isString(value)) {
            return (StringAtom) value;
        } else {
            throw new TypeMismatchException(SemblanceType.STRING, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link SymbolAtom}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link SymbolAtom}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link SymbolAtom}
     */
    public static SymbolAtom asSymbol(final LispValue value) {
        if (isSymbol(value)) {
            return (SymbolAtom) value;
        } else {
            throw new TypeMismatchException(SemblanceType.SYMBOL, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link Vector}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link Vector}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link Vector}
     */
    public static Vector asVector(final LispValue value) {
        if (isVector(value)) {
            return (Vector) value;
        } else {
            throw new TypeMismatchException(SemblanceType.VECTOR, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link SList}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link SList}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link SList}
     */
    public static SList asSList(final LispValue value) {
        if (isList(value)) {
            return (SList) value;
        } else {
            throw new TypeMismatchException(SemblanceType.LIST, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link ReturnValue}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link ReturnValue}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link ReturnValue}
     */
    public static ReturnValue asReturnValue(final LispValue value) {
        if (value.getType() == SemblanceType.RETURN) {
            return (ReturnValue) value;
        } else {
            throw new TypeMismatchException(SemblanceType.RETURN, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link SMap}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link SMap}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link SMap}
     */
    public static SMap asSMap(final LispValue value) {
        if (isMap(value)) {
            return (SMap) value;
        } else {
            throw new TypeMismatchException(SemblanceType.MAP, value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link OpaqueValue}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link OpaqueValue}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link OpaqueValue}
     */
    public static OpaqueValue<?> asOpaque(final LispValue value) {
        if (isOpaque(value)) {
            return (OpaqueValue) value;
        } else {
            throw new TypeMismatchException(SemblanceType.OPAQUE, value, getSourceInfo(value));
        }
    }

    /**
     * Converts any {@link LispCollection} to {@link SList}
     *
     * @param value collection to be converted to {@link SList}
     * @return Collection as SList.
     */
    public static SList toSList(final LispCollection value) {
        if (isList(value)) {
            return (SList) value;
        } else if (isVector(value)) {
            return new SList(value.getCollection());
        } else {
            throw new TypeMismatchException("Collection", value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link LispCollection}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link LispCollection}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link LispCollection}
     */
    public static LispCollection asCollection(final LispValue value) {
        if (value instanceof LispCollection) {
            return (LispCollection) value;
        } else {
            throw new TypeMismatchException("Collection", value, getSourceInfo(value));
        }
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link LispCallable}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link LispCallable}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link LispCallable}
     */
    public static LispCallable asCallable(final LispValue value) {
        if (value instanceof LispCallable) {
            return (LispCallable) value;
        } else {
            throw new TypeMismatchException("Callable", value, getSourceInfo(value));
        }
    }


    private static boolean canBeConvertedToJavaObject(final LispValue value) {
        return value instanceof JavaConvertible;
    }

    /**
     * Creates a {@link Vector} from an array of {@link LispValue}s
     *
     * @param values Values to include into vector
     * @return new {@link Vector}
     */
    public static Vector vector(final LispValue... values) {
        return new Vector(Iter.of(values).toList());
    }

    /**
     * Creates a {@link Vector} from an array of untyped objects. Internally uses {@link #atom(Object)} to convert
     * each value to {@link LispValue}
     *
     * @param values Values to include into vector
     * @return new {@link Vector}
     */
    public static Vector smartVector(final Object... values) {
        return vector(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    /**
     * Creates a {@link SList} from an array of {@link LispValue}s
     *
     * @param values Values to include into list
     * @return new {@link SList}
     */
    public static SList list(final LispValue... values) {
        return new SList(Iter.of(values).toList());
    }

    /**
     * Creates a {@link SList} from an array of untyped objects. Internally uses {@link #atom(Object)} to convert
     * each value to {@link LispValue}
     *
     * @param values Values to include into list
     * @return new {@link SList}
     */
    public static SList smartList(final Object... values) {
        return list(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link SList}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isList(final LispValue value) {
        return SemblanceType.LIST == value.getType();
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link SMap}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isMap(final LispValue value) {
        return SemblanceType.MAP == value.getType();
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link OpaqueValue}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isOpaque(final LispValue value) {
        return SemblanceType.OPAQUE == value.getType();
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link Vector}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isVector(final LispValue value) {
        return SemblanceType.VECTOR == value.getType();
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link SymbolAtom}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isSymbol(final LispValue value) {
        return SemblanceType.SYMBOL == value.getType();
    }

    /**
     * Verifies if provided {@link LispValue} is a function or macro
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isCallable(final LispValue value) {
        return SemblanceType.FUNCTION == value.getType() || SemblanceType.MACRO == value.getType() ||
                value instanceof LispCallable;
    }

    /**
     * Verifies if provided {@link LispValue} is {@link #F} or NIL collection
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isFalse(final LispValue value) {
        return Constants.FALSE.equals(value) || NilCollection.INSTANCE.equals(value);
    }

    /**
     * Verifies if provided {@link LispValue} is not {@link #F} or NIL collection
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isTrue(final LispValue value) {
        return !isFalse(value);
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link StringAtom}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isString(final LispValue value) {
        return value.getType() == SemblanceType.STRING;
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link Atom}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isAtom(final LispValue value) {
        return value instanceof Atom;
    }

    /**
     * Verifies if provided {@link LispValue} is an instance of {@link NumberAtom}
     *
     * @param value {@link LispValue} to check
     * @return true or false
     */
    public static boolean isNumber(final LispValue value) {
        return SemblanceType.FLOATIG_POINT == value.getType() || SemblanceType.INTEGER == value.getType();
    }

    /**
     * Converts collection to Java list of {@link LispValue}s
     *
     * @param in Instance of {@link LispCollection}
     * @return {@link List} of values in collection
     */
    public static List<LispValue> asList(final LispCollection in) {
        return Iter.of(in.iterator()).toList();
    }

    /**
     * Verifies if provided object is {@link LispCollection} but not a NIL collection
     *
     * @param collection object to verify
     * @return true or false
     */
    public static boolean isCollection(final Object collection) {
        return collection instanceof LispCollection && !((LispCollection) collection).isNil();
    }

    /**
     * Verifies if provided object is NIL collection
     *
     * @param collection object to verify
     * @return true or false
     */
    public static boolean isNilCollection(final Object collection) {
        return collection instanceof LispCollection && ((LispCollection) collection).isNil();
    }

    /**
     * Verifies {@link LispValue} type and converts it into {@link NumberAtom}, if possible. Throws {@link TypeMismatchException} if
     * value is not {@link NumberAtom}
     *
     * @param value {@link LispValue} to type cast
     * @return value casted to {@link NumberAtom}
     */
    public static NumberAtom asNumber(final LispValue value) {
        if (isNumber(value)) {
            return (NumberAtom) value;
        } else {
            throw new TypeMismatchException("Numeric", value, getSourceInfo(value));
        }
    }

    /**
     * Creates a {@link LongNumberAtom} from provided long value
     *
     * @param number long value
     * @return {@link LongNumberAtom}
     */
    public static NumberAtom longN(final long number) {
        return new LongNumberAtom(number);
    }

    /**
     * Creates a {@link BigIntegerNumberAtom} from provided string
     *
     * @param number string value
     * @return {@link BigIntegerNumberAtom}
     */
    public static NumberAtom bigN(final String number) {
        return new BigIntegerNumberAtom(new BigInteger(number));
    }

    /**
     * Creates a {@link BigIntegerNumberAtom} from provided {@link BigInteger} value
     *
     * @param number big integer value
     * @return {@link BigIntegerNumberAtom}
     */
    public static NumberAtom bigN(final BigInteger number) {
        return new BigIntegerNumberAtom(number);
    }

    /**
     * Creates a {@link BigIntegerNumberAtom} from provided long value
     *
     * @param number long value
     * @return {@link BigIntegerNumberAtom}
     */
    public static NumberAtom bigN(final long number) {
        return new BigIntegerNumberAtom(BigInteger.valueOf(number));
    }

    /**
     * Creates a {@link DoubleNumberAtom} from provided double value
     *
     * @param number double value
     * @return {@link DoubleNumberAtom}
     */
    public static NumberAtom doubleN(final double number) {
        return new DoubleNumberAtom(number);
    }

    /**
     * Converts boolean value to {@link SymbolAtom}
     *
     * @param value boolean value
     * @return either {@link #T} or {@link #F}
     */
    public static LispValue booleanAsAtom(final boolean value) {
        return value ? Constants.TRUE : Constants.FALSE;
    }

    /**
     * Returns clean string representation of {@link LispValue}. If value is {@link StringAtom}, returned value
     * will not be enclosed in quotes
     *
     * @param value value to convert to string
     * @return String representation of value
     */
    public static String cleanString(final LispValue value) {
        if (isString(value)) {
            return asString(value).getValue();
        } else {
            return value.repr();
        }
    }

    private enum FromJavaConverter implements Func<LispValue, Object> {
        INSTANCE;

        @Override
        public LispValue apply(final Object o) {
            return atom(o);
        }
    }

    /**
     * Converts LispValue to Java object, if LispValue supports {@link JavaConvertible} interface.
     */
    public enum ToJavaConverter implements Func<Object, LispValue> {
        INSTANCE;

        @Override
        public Object apply(final LispValue value) {
            if (canBeConvertedToJavaObject(value)) {
                return ((JavaConvertible) value).asJavaObject();
            } else {
                throw new IllegalArgumentException("Non-convertible value");
            }
        }
    }

    /**
     * Converts Java string to Symbol atom.
     */
    public enum StringToSymbol implements Func<LispValue, String> {
        INSTANCE;

        @Override
        public LispValue apply(final String s) {
            return symbol(s);
        }
    }
}
