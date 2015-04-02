package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.java.JavaConvertible;
import net.ninjacat.smooth.functions.Func;
import net.ninjacat.smooth.iterators.Iter;

import java.util.Collections;
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
            final LispValue value = context.evaluate(parameters.tail().head());
            return put(context.evaluate(parameters.head()), value);
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
        if (contents.containsKey(key)) {
            return contents.get(key);
        } else {
            return NilCollection.INSTANCE;
        }
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
}