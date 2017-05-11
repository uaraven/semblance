package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Basic implementation of namespace.
 */
public class BaseNamespace implements Namespace {

    private final SymbolAtom name;
    private final Map<SymbolAtom, LispValue> bindings;

    /**
     * Creates a new namespace
     *
     * @param name Name of this namespace.
     */
    public BaseNamespace(final SymbolAtom name) {
        this.name = name;
        bindings = new ConcurrentHashMap<>();
    }

    private BaseNamespace(final SymbolAtom name, final Map<SymbolAtom, LispValue> bindings) {
        this.name = name;
        this.bindings = new ConcurrentHashMap<>(bindings);
    }

    @Override
    public SymbolAtom getName() {
        return name;
    }

    @Override
    public Optional<LispValue> findSymbol(final SymbolAtom key) {
        if (bindings.containsKey(key)) {
            return Optional.ofNullable(bindings.get(key));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public void bind(final SymbolAtom key, final LispValue value) {
        bindings.put(key, value);
    }

    @Override
    public Namespace rename(final SymbolAtom newName) {
        return new BaseNamespace(newName, bindings);
    }

    @Override
    public Collection<Binding> getBindings() {
        final List<Binding> export = new ArrayList<>(bindings.size());
        for (final Map.Entry<SymbolAtom, LispValue> entry : bindings.entrySet()) {
            export.add(new Binding(entry.getKey(), entry.getValue()));
        }
        return export;
    }
}
