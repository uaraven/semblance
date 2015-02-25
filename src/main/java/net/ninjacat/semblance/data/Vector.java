package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.CollectionException;
import net.ninjacat.semblance.errors.CollectionIndexOutOfBoundsException;
import net.ninjacat.semblance.errors.ValueExpectedException;
import net.ninjacat.semblance.utils.ValueToString;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.iterators.Iter;

import java.util.Collections;
import java.util.List;

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
public class Vector extends LispCollection implements Function {

    private List<LispValue> collection;

    public Vector(SourceInfo sourceInfo, List<LispValue> collection) {
        super(sourceInfo);
        this.collection = Collections.unmodifiableList(collection);
    }

    public Vector(List<LispValue> collection) {
        this.collection = Collections.unmodifiableList(collection);
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
    public LispValue evaluate() {
        return this;
    }

    @Override
    public String repr() {
        return "[" + Iter.of(collection).map(ValueToString.REPR).mkStr(", ") + "]";
    }

    @Override
    public LispValue apply(LispCollection parameters) {
        if (parameters.isNil()) {
            throw new ValueExpectedException(getSourceInfo());
        } else {
            long index = Values.getLongValue(parameters.head());
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
            return new Vector(getSourceInfo(), collection.subList(1, collection.size()));
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
}
