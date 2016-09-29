package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.operations.*;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;
import net.ninjacat.semblance.errors.runtime.CollectionIndexOutOfBoundsException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.errors.runtime.ValueExpectedException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.smooth.functions.Func;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static net.ninjacat.semblance.data.collections.operations.Operation.*;
import static net.ninjacat.semblance.utils.Values.*;

/**
 * Parent class for lists and vectors
 */
@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
public abstract class LispCollection implements Iterable<LispValue>, DebugInfoProvider, JavaConvertible, LispCallable {

    private static final long serialVersionUID = -7182398711351245106L;
    private static final Map<SymbolAtom, ListOperation> OPERATIONS = new ConcurrentHashMap<>();

    private final SourceInfo sourceInfo;

    LispCollection() {
        this(SourceInfo.UNKNOWN);
    }

    LispCollection(final SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;

        addOperation(HEAD, new HeadOperation());
        addOperation(TAIL, new TailOperation());
        addOperation(LAST, new LastOperation());
        addOperation(TAKE, new TakeOperation());
        addOperation(DROP, new DropOperation());
        addOperation(REVERSE, new ReverseOperation());
        addOperation(SORT, new SortOperation());
        addOperation(SORTF, new SortFuncOperation());
        addOperation(MAP, new MapOperation());
        addOperation(FILTER, new FilterOperation());
        addOperation(APPEND, new AppendOperation());
        addOperation(PREPEND, new PrependOperation());
        addOperation(LENGTH, new LengthOperation());
    }

    static void addOperation(final Operation name, final ListOperation operation) {
        OPERATIONS.put(name.asSymbol(), operation);
    }

    /**
     * Returns first element of the collection.
     *
     * @return {@link LispValue} which is the first element of the collection.
     * @throws CollectionException if collection is empty.
     */
    public abstract LispValue head();

    /**
     * Returns collection of all elements except the first one. Type of new collection will be the same as of original collection.
     *
     * @return LispCollection containing all of the elements but the first. Will return
     * {@link NilCollection} if original collection contains only one element.
     * @throws CollectionException if original collection is empty.
     */
    public abstract LispCollection tail();

    /**
     * Returns number of elements in collection. Depending on implementation this can be very inefficient method
     *
     * @return number of elements in collection.
     */
    public abstract int length();

    /**
     * @return {@code true} if the collection is empty
     */
    public abstract boolean isNil();

    /**
     * @return This collection as list of values
     */
    public abstract List<LispValue> getCollection();

    /**
     * Creates new collection of the same type as this collection (vector or list) and fills it with supplied values.
     *
     * @param values List of values for new collection
     * @param <T>    Type of collection.
     * @return New collection.
     */
    public abstract <T extends LispCollection> T createSame(LispCollection values);

    /**
     * Creates new instance of the collection of this type holding supplied values.
     *
     * @param values Values for the collection.
     * @param <T>    Type of collection
     * @return New collection of the same type as this.
     */
    public abstract <T extends LispCollection> T createNew(List<LispValue> values);

    /**
     * Adds new elements to the end of a collection
     *
     * @param lispValues values to add
     * @return new collection
     */
    public abstract LispCollection append(final LispCollection lispValues);

    /**
     * Inserts new elements into beginning of a collection
     *
     * @param lispValues values to add
     * @return new collection
     */
    public abstract LispCollection prepend(final LispCollection lispValues);

    /**
     * Searches for an element inside the collection.
     *
     * @param itemToFind Element to look for.
     * @return index of element in the collection or -1 if element is not in the collection.
     */
    public int indexOf(@Nonnull final LispValue itemToFind) {
        return getCollection().indexOf(itemToFind);
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (parameters.isNil()) {
            throw new ValueExpectedException(getSourceInfo());
        } else {
            final LispValue value = context.evaluate(parameters.head());
            if (isSymbol(value)) {
                return executeOperation(context, context.evaluateList(parameters.tail()), value);
            } else {
                if (parameters.length() == 1) {
                    if (isNumber(value)) {
                        return getLispValue(value, parameters.getSourceInfo());
                    } else {
                        return this;
                    }
                } else {
                    final LispValue from = context.evaluate(parameters.head());
                    final LispValue to = context.evaluate(parameters.tail().head());
                    return slice(from, to);
                }
            }
        }
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    /**
     * Performs a slice on collection.
     *
     * @param from first index of new collection
     * @param to   last index of new collection
     * @return Sub-collection
     */
    public LispValue slice(final LispValue from, final LispValue to) {
        final int fromI = (int) getLongValue(from);
        final int toI = (int) getLongValue(to);
        return slice(fromI, toI);
    }

    /**
     * Same as {@link #slice(LispValue, LispValue)}, but uses integer indices
     *
     * @param from start index.
     * @param to   final index.
     * @return new collection.
     */
    public LispValue slice(final int from, final int to) {
        return createNew(getCollection().subList(from, to + 1));
    }

    /**
     * Applies callable for each element in collection and creates a new collection of the same type as
     * this collection containing result of function application.
     *
     * @param context    Execution context for function application.
     * @param applicator Function to apply to elements of collection.
     * @return New collection of results of function application.
     */
    public LispCollection foreach(@Nonnull final Context context, @Nonnull final LispCallable applicator) {
        final List<LispValue> result = new ArrayList<>(getCollection().size());
        for (final LispValue item : getCollection()) {
            result.add(applicator.apply(context, list(item)));
        }
        return createNew(result);
    }

    /**
     * Get an element by its index.
     *
     * @param index Index of the element.
     * @return Value of the element.
     */
    public LispValue get(final int index) {
        try {
            return getCollection().get(normalize(index));
        } catch (final IndexOutOfBoundsException ex) {
            throw new CollectionIndexOutOfBoundsException(index, getCollection().size(), SourceInfo.UNKNOWN, ex);
        }
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getType() == getType()) {
            return getCollection().equals(asCollection(other).getCollection()) ? 0 : 1;
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

    private int normalize(final int index) {
        if (index >= 0) {
            return index;
        } else {
            return length() + index;
        }
    }

    private LispValue executeOperation(@Nonnull final Context context,
                                       @Nonnull final LispCollection parameters,
                                       @Nonnull final LispValue value) {
        final SymbolAtom keyword = asSymbol(value);
        if (OPERATIONS.containsKey(keyword)) {
            return OPERATIONS.get(keyword).apply(context, this, parameters);
        } else {
            throw new net.ninjacat.semblance.errors.runtime.UnsupportedOperationException(keyword.repr(), keyword.getSourceInfo());
        }
    }

    private LispValue getLispValue(final LispValue value, final SourceInfo sourceI) {
        final long index = getLongValue(value);
        try {
            return get((int) index);
        } catch (final IndexOutOfBoundsException ex) {
            throw new CollectionIndexOutOfBoundsException(index, getCollection().size(), sourceI, ex);
        }
    }

    enum ValueToString implements Func<String, LispValue> {
        REPR;

        @Override
        public String apply(final LispValue lispValue) {
            return lispValue.repr();
        }
    }
}
