package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;

/**
 * Parent class for lists and vectors
 */
public abstract class LispCollection implements LispValue, DebugInfoProvider, JavaConvertible {

    private final SourceInfo sourceInfo;

    public LispCollection() {
        this.sourceInfo = SourceInfo.UNKNOWN;
    }

    public LispCollection(SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;
    }

    @Override
    public LispValue self() {
        return this;
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    /**
     * Returns first element of the collection.
     *
     * @return {@link LispValue} which is the first element of the collection.
     * @throws net.ninjacat.semblance.errors.CollectionException if collection is empty.
     */
    public abstract LispValue head();

    /**
     * Returns collection of all elements except the first one. Type of new collection will be the same as of original collection.
     *
     * @return {@link net.ninjacat.semblance.data.LispCollection} containing all of the elements but the first. Will return
     * {@link net.ninjacat.semblance.data.NilCollection} if original collection contains only one element.
     * @throws net.ninjacat.semblance.errors.CollectionException if original collection is empty.
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
}
