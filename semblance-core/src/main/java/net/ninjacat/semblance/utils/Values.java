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
 * Various utils simplifying value manipulations
 */
@SuppressWarnings({"ConstantNamingConvention", "JavaDoc"})
public final class Values {
    public static final SymbolAtom T = symbol("T");
    public static final SymbolAtom F = symbol("F");

    private Values() {
    }

    public static long getLongValue(final LispValue atom) {
        if (atom instanceof LongNumberAtom) {
            return ((LongNumberAtom) atom).asJavaObject();
        } else {
            throw new TypeMismatchException(SemblanceType.INTEGER, atom, getSourceInfo(atom));
        }
    }

    public static SourceInfo getSourceInfo(final LispValue atom) {
        if (atom instanceof DebugInfoProvider) {
            return ((DebugInfoProvider) atom).getSourceInfo();
        } else {
            return SourceInfo.UNKNOWN;
        }

    }

    public static LispValue number(final String value) {
        return NumberAtom.make(value);
    }

    public static LispValue number(final long value) {
        return new LongNumberAtom(value);
    }

    public static LispValue number(final double value) {
        return new DoubleNumberAtom(value);
    }

    @SuppressWarnings("QuestionableName")
    public static LispValue string(final String value) {
        return new StringAtom(value);
    }

    public static SymbolAtom symbol(final String value) {
        return new SymbolAtom(value);
    }

    public static SymbolAtom symbol(final String value, final SourceInfo sourceInfo) {
        return new SymbolAtom(value, sourceInfo);
    }

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

    public static StringAtom asString(final LispValue value) {
        if (isString(value)) {
            return (StringAtom) value;
        } else {
            throw new TypeMismatchException(SemblanceType.STRING, value, getSourceInfo(value));
        }
    }

    public static SymbolAtom asSymbol(final LispValue value) {
        if (isSymbol(value)) {
            return (SymbolAtom) value;
        } else {
            throw new TypeMismatchException(SemblanceType.SYMBOL, value, getSourceInfo(value));
        }
    }

    public static Vector asVector(final LispValue value) {
        if (isVector(value)) {
            return (Vector) value;
        } else {
            throw new TypeMismatchException(SemblanceType.VECTOR, value, getSourceInfo(value));
        }
    }

    public static SList asSList(final LispValue value) {
        if (isList(value)) {
            return (SList) value;
        } else {
            throw new TypeMismatchException(SemblanceType.LIST, value, getSourceInfo(value));
        }
    }

    public static ReturnValue asReturnValue(final LispValue value) {
        if (value.getType() == SemblanceType.RETURN) {
            return (ReturnValue) value;
        } else {
            throw new TypeMismatchException(SemblanceType.RETURN, value, getSourceInfo(value));
        }
    }

    public static SMap asSMap(final LispValue value) {
        if (isMap(value)) {
            return (SMap) value;
        } else {
            throw new TypeMismatchException(SemblanceType.MAP, value, getSourceInfo(value));
        }
    }

    /**
     * Converts any {@link LispCollection} to {@link SList}
     *
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

    public static LispCollection asCollection(final LispValue value) {
        if (value instanceof LispCollection) {
            return (LispCollection) value;
        } else {
            throw new TypeMismatchException("Collection", value, getSourceInfo(value));
        }
    }

    public static Callable asCallable(final LispValue value) {
        if (value instanceof Callable) {
            return (Callable) value;
        } else {
            throw new TypeMismatchException("Callable", value, getSourceInfo(value));
        }
    }

    public static boolean canBeConvertedToJavaObject(final LispValue value) {
        return value instanceof JavaConvertible;
    }

    public static Vector vector(final LispValue... values) {
        return new Vector(Iter.of(values).toList());
    }

    public static Vector smartVector(final Object... values) {
        return vector(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    public static SList list(final LispValue... values) {
        return new SList(Iter.of(values).toList());
    }

    public static SList smartList(final Object... values) {
        return list(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    public static boolean isList(final LispValue value) {
        return SemblanceType.LIST == value.getType();
    }

    public static boolean isMap(final LispValue value) {
        return SemblanceType.MAP == value.getType();
    }

    public static boolean isVector(final LispValue value) {
        return SemblanceType.VECTOR == value.getType();
    }

    public static boolean isSymbol(final LispValue value) {
        return SemblanceType.SYMBOL == value.getType();
    }

    public static boolean isCallable(final LispValue value) {
        return SemblanceType.FUNCTION == value.getType() || SemblanceType.MACRO == value.getType() ||
                value instanceof Callable;
    }

    public static boolean isFalse(final LispValue value) {
        return Constants.FALSE.equals(value) || NilCollection.INSTANCE.equals(value);
    }

    public static boolean isTrue(final LispValue value) {
        return !isFalse(value);
    }

    public static boolean isString(final LispValue value) {
        return value.getType() == SemblanceType.STRING;
    }

    public static boolean isAtom(final LispValue value) {
        return value instanceof Atom;
    }

    public static List<LispValue> asList(final LispCollection in) {
        return Iter.of(in.iterator()).toList();
    }

    public static boolean isCollection(final Object collection) {
        return collection instanceof LispCollection && !((LispCollection) collection).isNil();
    }

    public static boolean isNilCollection(final Object collection) {
        return collection instanceof LispCollection && ((LispCollection) collection).isNil();
    }

    public static NumberAtom asNumber(final LispValue value) {
        if (isNumber(value)) {
            return (NumberAtom) value;
        } else {
            throw new TypeMismatchException("Numeric", value, getSourceInfo(value));
        }
    }

    public static boolean isNumber(final LispValue value) {
        return SemblanceType.FLOATIG_POINT == value.getType() || SemblanceType.INTEGER == value.getType();
    }

    public static NumberAtom longN(final long number) {
        return new LongNumberAtom(number);
    }

    public static NumberAtom bigN(final String number) {
        return new BigIntegerNumberAtom(new BigInteger(number));
    }

    public static NumberAtom bigN(final BigInteger number) {
        return new BigIntegerNumberAtom(number);
    }

    public static NumberAtom bigN(final long number) {
        return new BigIntegerNumberAtom(BigInteger.valueOf(number));
    }

    public static NumberAtom doubleN(final double number) {
        return new DoubleNumberAtom(number);
    }

    public static long fromNumber(final NumberAtom number) {
        return (Long) number.asJavaObject();
    }

    public static LispValue booleanAsAtom(final boolean value) {
        return value ? Constants.TRUE : Constants.FALSE;
    }

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
