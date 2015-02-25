package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.CollectionException;

/**
 * Empty collection
 */
public class NilCollection extends LispCollection {
    private static final String NAME = "NIL";

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
}
