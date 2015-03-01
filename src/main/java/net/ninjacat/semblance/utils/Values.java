package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.*;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.TypeMismatchException;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.semblance.java.Symbol;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import java.util.List;

/**
 * Various utils simplifying value manipulations
 */
public final class Values {
    public static final SymbolAtom T = symbol("T");
    public static final SymbolAtom F = symbol("F");

    private Values() {
    }

    public static long getLongValue(LispValue atom) {
        if (atom instanceof LongNumberAtom) {
            return ((LongNumberAtom) atom).asJavaObject();
        } else {
            throw new TypeMismatchException(SemblanceType.INTEGER, atom.getType(), getSourceInfo(atom));
        }
    }

    public static SourceInfo getSourceInfo(LispValue atom) {
        if (atom instanceof DebugInfoProvider) {
            return ((DebugInfoProvider) atom).getSourceInfo();
        } else {
            return SourceInfo.UNKNOWN;
        }

    }

    public static LispValue number(String value) {
        return NumberAtom.make(value);
    }

    public static LispValue number(long value) {
        return new LongNumberAtom(value);
    }

    public static LispValue string(String value) {
        return new StringAtom(value);
    }

    public static SymbolAtom symbol(String value) {
        return new SymbolAtom(value);
    }

    public static SymbolAtom symbol(String value, SourceInfo sourceInfo) {
        return new SymbolAtom(value, sourceInfo);
    }

    public static LispValue atom(Object value) {
        if (value instanceof String) {
            return string(value.toString());
        } else if (value instanceof Long) {
            return number((Long) value);
        } else if (value instanceof Symbol) {
            return symbol(((Symbol) value).getValue());
        } else {
            throw new IllegalArgumentException(String.format("Cannot convert %s to atom", value));
        }
    }

    public static SymbolAtom asSymbol(LispValue value) {
        if (isSymbol(value)) {
            return (SymbolAtom) value;
        } else {
            throw new TypeMismatchException(SemblanceType.SYMBOL, value.getType(), getSourceInfo(value));
        }
    }

    public static SList asSList(LispValue value) {
        if (isList(value)) {
            return (SList) value;
        } else {
            throw new TypeMismatchException(SemblanceType.LIST, value.getType(), getSourceInfo(value));
        }
    }

    public static Callable asCallable(LispValue value) {
        return (Callable) value;
    }

    public static boolean canBeConvertedToJavaObject(LispValue value) {
        return value instanceof JavaConvertible;
    }

    public static Vector vector(LispValue... values) {
        return new Vector(Iter.of(values).toList());
    }

    public static Vector smartVector(Object... values) {
        return vector(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    public static SList list(LispValue... values) {
        return new SList(Iter.of(values).toList());
    }

    public static SList smartList(Object... values) {
        return list(Iter.of(values).map(FromJavaConverter.INSTANCE).toArray(new LispValue[values.length]));
    }

    public static boolean isList(LispValue value) {
        return value.getType() == SemblanceType.LIST;
    }

    public static boolean isSymbol(LispValue value) {
        return value.getType() == SemblanceType.SYMBOL;
    }

    public static boolean isCallable(LispValue value) {
        return value.getType() == SemblanceType.FUNCTION || value.getType() == SemblanceType.MACRO;
    }

    public static boolean isAtom(LispValue value) {
        return value instanceof Atom;
    }

    public static List<LispValue> asList(LispCollection in) {
        return Iter.of(in.iterator()).toList();
    }

    public static boolean isNilCollection(Object collection) {
        return collection instanceof LispCollection && ((LispCollection) collection).isNil();
    }

    private enum FromJavaConverter implements Func<LispValue, Object> {
        INSTANCE;

        @Override
        public LispValue apply(Object o) {
            return atom(o);
        }
    }

    public static enum ToJavaConverter implements Func<Object, LispValue> {
        INSTANCE;

        @Override
        public Object apply(LispValue value) {
            if (canBeConvertedToJavaObject(value)) {
                return ((JavaConvertible) value).asJavaObject();
            } else {
                throw new IllegalArgumentException("Non-convertible value");
            }
        }
    }

}
