package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.smooth.functions.Func;

import java.util.List;

/**
 * Parent class for lists and vectors
 */
public abstract class LispCollection implements Iterable<LispValue>, LispValue, DebugInfoProvider, JavaConvertible {

    private final SourceInfo sourceInfo;

    LispCollection() {
        sourceInfo = SourceInfo.UNKNOWN;
    }

    LispCollection(final SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;
    }

    /**
     * Returns first element of the collection.
     *
     * @return {@link LispValue} which is the first element of the collection.
     * @throws net.ninjacat.semblance.errors.runtime.CollectionException if collection is empty.
     */
    public abstract LispValue head();

    /**
     * Returns collection of all elements except the first one. Type of new collection will be the same as of original collection.
     *
     * @return LispCollection containing all of the elements but the first. Will return
     * {@link NilCollection} if original collection contains only one element.
     * @throws net.ninjacat.semblance.errors.runtime.CollectionException if original collection is empty.
     */
    public abstract LispCollection tail();

    /**
     * Returns number of elements in collection. Depending on implementation this can be very inefficient method
     *
     * @return number of elements in collection.
     */
    public abstract long length();

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

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    protected enum ValueToString implements Func<String, LispValue> {
        REPR;

        @Override
        public String apply(final LispValue lispValue) {
            return lispValue.repr();
        }
    }
}
