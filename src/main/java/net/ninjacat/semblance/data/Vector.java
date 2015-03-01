package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;
import net.ninjacat.semblance.errors.runtime.CollectionIndexOutOfBoundsException;
import net.ninjacat.semblance.errors.runtime.ValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.iterators.Iter;

import java.util.*;

import static java.util.Collections.unmodifiableList;

/**
 * Vector collection.
 * <br/>
 * Vector allows indexed access to its elements and also efficiently calculates its size
 * <pre>
 *     Semblance syntax:
 *
 *     [elem1, elem2, ..., elemN]
 *     or
 *     (vector elem1, elem2, ..., elemN)
 *
 *     if vector is bound to a variable v then following code can be used to access its elements by index:
 *     (v i), where
 *     i is index
 *     (v length) returns vector size
 * </pre>
 */
public class Vector extends LispCollection implements Callable {

    private static final SymbolAtom NAME = new SymbolAtom("--vector-get");

    private List<LispValue> collection;

    public Vector(Collection<LispValue> collection, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.collection = unmodifiableList(new ArrayList<>(collection));
    }

    public Vector(Collection<LispValue> collection) {
        this.collection = unmodifiableList(new ArrayList<>(collection));
    }

    @Override
    public List<?> asJavaObject() {
        try {
            return Iter.of(collection).map(Values.ToJavaConverter.INSTANCE).toList();
        } catch (IllegalArgumentException ex) {
            throw new CollectionException("Cannot create Java representation", getSourceInfo(), ex);
        }
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.VECTOR;
    }

    @Override
    public String repr() {
        return "[" + Iter.of(collection).map(ValueToString.REPR).mkStr(", ") + "]";
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
        if (parameters.isNil()) {
            throw new ValueExpectedException(getSourceInfo());
        } else {
            LispValue value = context.evaluate(parameters.head());
            long index = Values.getLongValue(value);
            try {
                return collection.get((int) index);
            } catch (IndexOutOfBoundsException ex) {
                throw new CollectionIndexOutOfBoundsException(index, collection.size(), parameters.getSourceInfo(), ex);
            }
        }
    }

    public LispValue get(int index) {
        return collection.get(index);
    }

    @Override
    public LispValue head() {
        if (isNil()) {
            throw new CollectionException("Cannot get head of empty vector", getSourceInfo());
        } else {
            return collection.get(0);
        }
    }

    @Override
    public LispCollection tail() {
        if (isNil()) {
            throw new CollectionException("Cannot get tail of empty vector", getSourceInfo());
        } else if (collection.size() == 1) {
            return new NilCollection(getSourceInfo());
        } else {
            return new Vector(collection.subList(1, collection.size()), getSourceInfo());
        }
    }

    @Override
    public long length() {
        return collection.size();
    }

    @Override
    public boolean isNil() {
        return collection.isEmpty();
    }

    @Override
    public Vector createSame(LispCollection values) {
        return new Vector(values.getCollection(), getSourceInfo());
    }

    @Override
    public List<LispValue> getCollection() {
        return Collections.unmodifiableList(collection);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (isNil() && Values.isNilCollection(o)) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Vector vector = (Vector) o;

        if (!collection.equals(vector.collection)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return collection.hashCode();
    }

    @Override
    public String toString() {
        return "Vector{" + collection + '}';
    }

    @Override
    public Iterator<LispValue> iterator() {
        return collection.listIterator();
    }

    @Override
    public SymbolAtom name() {
        return null;
    }
}
