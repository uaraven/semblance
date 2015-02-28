package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.CollectionException;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Empty collection
 */
public class NilCollection extends LispCollection {
    public static final NilCollection INSTANCE = new NilCollection();
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

    public NilCollection() {
    }

    public NilCollection(SourceInfo sourceInfo) {
        super(sourceInfo);
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
    public long length() {
        return 0;
    }

    @Override
    public boolean isNil() {
        return true;
    }

    @Override
    public Object asJavaObject() {
        return null;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.LIST;
    }

    @Override
    public LispValue evaluate() {
        return this;
    }

    @Override
    public String repr() {
        return NAME;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LispCollection)) {
            return false;
        }
        LispCollection other = (LispCollection) obj;
        return other.isNil();
    }

    @Override
    public int hashCode() {
        return 17;
    }

    @Override
    public Iterator<LispValue> iterator() {
        return NIL_ITERATOR;
    }
}
