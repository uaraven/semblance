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
public class SList extends LispCollection implements Callable {

    private static final SymbolAtom NAME = new SymbolAtom("--list-get");

    private List<LispValue> collection;

    public SList(Collection<LispValue> collection, SourceInfo sourceInfo) {
        super(sourceInfo);
        this.collection = unmodifiableList(new LinkedList<>(collection));
    }

    public SList(Collection<LispValue> collection) {
        this.collection = unmodifiableList(new LinkedList<>(collection));
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
        return SemblanceType.LIST;
    }

    @Override
    public String repr() {
        return "(" + Iter.of(collection).map(ValueToString.REPR).mkStr(", ") + ")";
    }

    @Override
    public SymbolAtom name() {
        return NAME;
    }

    @Override
    public LispValue apply(Context context, LispCollection parameters) {
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
            throw new CollectionException("Cannot get head of empty list", getSourceInfo());
        } else {
            return collection.get(0);
        }
    }

    @Override
    public LispCollection tail() {
        if (isNil()) {
            throw new CollectionException("Cannot get tail of empty list", getSourceInfo());
        } else if (collection.size() == 1) {
            return new NilCollection(getSourceInfo());
        } else {
            return new SList(collection.subList(1, collection.size()), getSourceInfo());
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
    public SList createSame(LispCollection values) {
        return new SList(values.getCollection(), getSourceInfo());
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

        SList list = (SList) o;

        if (!collection.equals(list.collection)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return collection.hashCode();
    }

    @Override
    public String toString() {
        return "List{" + collection + '}';
    }

    @Override
    public Iterator<LispValue> iterator() {
        return collection.iterator();
    }

}
