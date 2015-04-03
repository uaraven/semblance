package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;
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
public class Vector extends LispCollection {

    private final List<LispValue> collection;

    /**
     * Creates new vector.
     *
     * @param collection Collection to be wrapped in the vector.
     * @param sourceInfo Source code information.
     */

    public Vector(final Collection<LispValue> collection, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.collection = unmodifiableList(new ArrayList<>(collection));
    }

    /**
     * Creates new vector.
     *
     * @param collection Collection to be wrapped in the vector.
     */
    public Vector(final Collection<LispValue> collection) {
        this.collection = unmodifiableList(new ArrayList<>(collection));
    }

    @Override
    public List<?> asJavaObject() {
        try {
            return Iter.of(collection).map(Values.ToJavaConverter.INSTANCE).toList();
        } catch (final IllegalArgumentException ex) {
            throw new CollectionException("Cannot create Java representation", getSourceInfo(), ex);
        }
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.VECTOR;
    }

    @Override
    public String repr() {
        return "[" + Iter.of(collection).map(ValueToString.REPR).mkStr(" ") + "]";
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
        } else if (1 == collection.size()) {
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

    @SuppressWarnings("unchecked")
    @Override
    public Vector createSame(final LispCollection values) {
        return new Vector(values.getCollection(), getSourceInfo());
    }

    @Override
    public List<LispValue> getCollection() {
        return Collections.unmodifiableList(collection);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T extends LispCollection> T createNew(final List<LispValue> values) {
        return (T) new Vector(values);
    }

    @SuppressWarnings("all")
    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;

        if (isNil() && Values.isNilCollection(o)) return true;

        if (o == null || getClass() != o.getClass()) return false;

        final Vector vector = (Vector) o;

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
