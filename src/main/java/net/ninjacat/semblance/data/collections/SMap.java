package net.ninjacat.semblance.data.collections;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;
import net.ninjacat.smooth.utils.Option;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Map implementation for Semblance
 */
@SuppressWarnings("ClassNamingConvention")
public class SMap implements DebugInfoProvider, Callable, JavaConvertible {

    private static final SymbolAtom NAME = new SymbolAtom("--map-access");
    private final SourceInfo sourceInfo;
    private final Map<LispValue, LispValue> contents;

    /**
     * Creates new instance of map
     *
     * @param data       Representation of the map
     * @param sourceInfo source code information
     */
    public SMap(final Map<LispValue, LispValue> data, final SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;
        contents = new ConcurrentHashMap<>(data);
    }

    @Override
    public SymbolAtom name() {
        return NAME;
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        if (parameters.length() == 2) {
            final LispValue key = context.evaluate(parameters.head());
            final LispValue value = context.evaluate(parameters.tail().head());
            return put(key, value);
        } else if (parameters.length() == 1) {
            final LispValue key = context.evaluate(parameters.head());
            return get(key);
        } else {
            return this;
        }
    }

    /**
     * Puts a value into map
     *
     * @param key   Map key
     * @param value Value to put
     * @return Stored value
     */
    public LispValue put(final LispValue key, final LispValue value) {
        contents.put(key, value);
        return value;
    }

    /**
     * Retrieves value from map
     *
     * @param key Map key
     * @return Value for given key or {@link NilCollection#INSTANCE}
     */
    public LispValue get(final LispValue key) {
        return Option.of(contents.get(key)).or(NilCollection.INSTANCE);
    }

    /**
     * Checks if this map contains the key.
     *
     * @param key Key to check.
     * @return {@code true} or {@code false}
     */
    public boolean contains(final LispValue key) {
        return contents.containsKey(key);
    }

    /**
     * @return List of map's keys.
     */
    public SList keys() {
        return new SList(contents.keySet());
    }

    /**
     * @return List of map's values.
     */
    public SList values() {
        return new SList(contents.values());
    }


    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    @Override
    public Object asJavaObject() {
        return Collections.unmodifiableMap(contents);
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.MAP;
    }

    @Override
    public String repr() {
        return "{" + Iter.of(contents.entrySet()).map(new Func<String, Map.Entry<LispValue, LispValue>>() {
            @Override
            public String apply(final Map.Entry<LispValue, LispValue> entry) {
                return entry.getKey().repr() + " " + entry.getValue().repr();
            }
        }).mkStr(" ") + "}";
    }

    /**
     * @return Number of entries in the map.
     */
    public int length() {
        return contents.size();
    }

    /**
     * Map literal cannot be evaluated during the reading phase. All function calls/atoms are stored in the map as
     * literals. This method is called when map object is accessed in the execution context and evaluates all
     * keys and values within this context. This only happens once as map literal is evaluated.
     *
     * @param context Evaluation context.
     * @return this map with updated keys and values.
     */
    public LispValue evaluateValues(final Context context) {
        final Map<LispValue, LispValue> updated = new HashMap<>();

        synchronized (contents) {
            for (final Map.Entry<LispValue, LispValue> entry : contents.entrySet()) {
                updated.put(context.evaluate(entry.getKey()), context.evaluate(entry.getValue()));
            }
            contents.clear();
            contents.putAll(updated);
        }

        return this;
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getType() == getType()) {
            return contents.equals(((SMap) other).contents) ? 0 : 1;
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        final SMap sMap = (SMap) o;

        return contents.equals(sMap.contents);

    }

    @Override
    public int hashCode() {
        return contents.hashCode();
    }
}
