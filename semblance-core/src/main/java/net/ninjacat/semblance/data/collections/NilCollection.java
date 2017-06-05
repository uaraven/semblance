package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.CollectionException;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Empty collection
 */
public class NilCollection extends SList {
    public static final NilCollection INSTANCE = new NilCollection();
    private static final long serialVersionUID = -912504802815726996L;
    private static final String NAME = "NIL";
    private static final Iterator<LispValue> NIL_ITERATOR = new Iterator<LispValue>() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public LispValue next() {
            throw new NoSuchElementException();
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    };

    NilCollection() {
        super(Collections.emptyList());
    }

    NilCollection(final SourceInfo sourceInfo) {
        super(Collections.emptyList(), sourceInfo);
    }

    @Override
    public LispValue head() {
        throw new CollectionException("Cannot get head of " + NAME, getSourceInfo());
    }

    @Override
    public LispCollection tail() {
        throw new CollectionException("Cannot get tail of " + NAME, getSourceInfo());
    }

    @Override
    public int length() {
        return 0;
    }

    @Override
    public boolean isNil() {
        return true;
    }

    @Override
    public SList createSame(final LispCollection values) {
        return NilCollection.INSTANCE;
    }

    @Override
    public List<LispValue> getCollection() {
        return Collections.emptyList();
    }

    @Override
    public List asJavaObject() {
        return null;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.LIST;
    }

    @Override
    public String repr() {
        return NAME;
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof LispCollection)) {
            return false;
        }
        final LispCollection other = (LispCollection) obj;
        return other.isNil();
    }

    @Override
    public int hashCode() {
        return 17;
    }

    @Nonnull
    @Override
    public Iterator<LispValue> iterator() {
        return NIL_ITERATOR;
    }
}
