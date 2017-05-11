package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;

import java.util.*;
import java.util.stream.Collectors;

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

    private static final long serialVersionUID = -2646116337995207839L;

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
            return collection.stream().map(Values.TO_JAVA).collect(Collectors.toList());
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
        return printIt();
    }

    @Override
    public String printIt() {
        return "[" + collection.stream().map(LispValue::printIt).collect(Collectors.joining(" ")) + "]";
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
    public int length() {
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

    @Override
    public LispCollection append(final LispCollection lispValues) {
        final List<LispValue> temp = new ArrayList<>(collection);
        temp.addAll(lispValues.getCollection());
        return createNew(temp);
    }

    @Override
    public LispCollection prepend(final LispCollection lispValues) {
        final List<LispValue> temp = new ArrayList<>(lispValues.getCollection());
        temp.addAll(collection);
        return createNew(temp);
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

    /**
     * Vector literal cannot be evaluated during the reading phase. All function calls/atoms are stored in the vector as
     * literals. This method is called when vector object is accessed in the execution context and evaluates all
     * keys and values within this context. This only happens once as vector literal is evaluated.
     *
     * @param context Evaluation context.
     * @return new Vector with updated keys and values.
     */

    public LispValue evaluateValues(final Context context) {
        return new Vector(collection.stream().map(context::evaluate).collect(Collectors.toList()), getSourceInfo());
    }
}
