package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.Collections.unmodifiableList;

/**
 * List collection.
 * <br/>
 * List allows fast head and tail operations, but slow indexed access to its elements. Size calculation might be inefficient
 * <pre>
 *     Semblance syntax:
 *
 *     (elem1, elem2, ..., elemN)
 *     or
 *     (list elem1, elem2, ..., elemN) s-expression
 *
 *     if list is bound to a variable L then following code can be used to access its elements by index:
 *     (L i), where
 *     i is index
 *     (L length) returns list size
 * </pre>
 */
public class SList extends LispCollection {

    private static final long serialVersionUID = -228096389065534835L;

    private static final SymbolAtom NAME = new SymbolAtom("--list-get");

    private final List<LispValue> collection;

    /**
     * Creates new list.
     *
     * @param collection Collection to be wrapped in the list.
     * @param sourceInfo Source code information.
     */
    public SList(final Collection<LispValue> collection, final SourceInfo sourceInfo) {
        super(sourceInfo);
        this.collection = unmodifiableList(new LinkedList<>(collection));
    }

    /**
     * Creates new list.
     *
     * @param collection Collection to be wrapped in the list.
     */
    public SList(final Collection<LispValue> collection) {
        this.collection = unmodifiableList(new LinkedList<>(collection));
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
        return SemblanceType.LIST;
    }

    @Override
    public String repr() {
        return "(" + collection.stream().map(LispValue::repr).collect(Collectors.joining(" ")) + ")";
    }

    @Override
    public String printIt() {
        return "(" + collection.stream().map(LispValue::printIt).collect(Collectors.joining(" ")) + ")";
    }

    @Override
    public SymbolAtom name() {
        return NAME;
    }

    @Override
    public LispValue head() {
        if (isNil()) {
            throw new CollectionException("Cannot get head of empty list", getSourceInfo());
        } else {
            return collection.get(0);
        }
    }

    @Override
    public LispCollection tail() {
        if (isNil()) {
            throw new CollectionException("Cannot get tail of empty list", getSourceInfo());
        } else if (1 == collection.size()) {
            return new NilCollection(getSourceInfo());
        } else {
            return new SList(collection.subList(1, collection.size()), getSourceInfo());
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
    public SList createSame(final LispCollection values) {
        return new SList(values.getCollection(), getSourceInfo());
    }

    @Override
    public List<LispValue> getCollection() {
        return Collections.unmodifiableList(collection);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T extends LispCollection> T createNew(final List<LispValue> values) {
        return (T) new SList(values);
    }

    @Override
    public LispCollection append(final LispCollection lispValues) {
        final List<LispValue> temp = new LinkedList<>(collection);
        temp.addAll(lispValues.getCollection());
        return createNew(temp);
    }

    @Override
    public LispCollection prepend(final LispCollection lispValues) {
        final List<LispValue> temp = new LinkedList<>(lispValues.getCollection());
        temp.addAll(collection);
        return createNew(temp);
    }

    @SuppressWarnings("all")
    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;

        if (isNil() && Values.isNilCollection(o)) return true;

        if (null == o || getClass() != o.getClass()) return false;

        final SList list = (SList) o;

        if (!collection.equals(list.collection)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return collection.hashCode();
    }

    @Override
    public String toString() {
        return printIt();
    }

    @Nonnull
    @Override
    public Iterator<LispValue> iterator() {
        return collection.iterator();
    }

}
